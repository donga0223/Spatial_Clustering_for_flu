library(dplyr)
library(tidyr)
library(ggplot2)
library(scoringutils)
library(purrr)

forecast_est_data <- function(out, i) {
  out <- out %>%
    mutate(
      no = as.factor(i),
      horizon = horizon + 1,
      target_end_date = as.Date(target_end_date),
      reference_date = as.Date(reference_date)
    )
  
  est_low <- out %>%
    filter(output_type_id == 0.025) %>%
    mutate(est_low = value) %>%
    dplyr::select(-output_type_id, -value)
  
  est_median <- out %>%
    filter(output_type_id == 0.5) %>%
    mutate(est_median = value) %>%
    dplyr::select(-output_type_id, -value)
  
  est_high <- out %>%
    filter(output_type_id == 0.975) %>%
    mutate(est_high = value) %>%
    dplyr::select(-output_type_id, -value)
  
  est_low %>%
    left_join(
      est_median,
      by = c("location", "reference_date", "horizon",
             "target_end_date", "target", "output_type", "no")
    ) %>%
    left_join(
      est_high,
      by = c("location", "reference_date", "horizon",
             "target_end_date", "target", "output_type", "no")
    )
}

all_ribbon_data <- function(date_list,
                            output_folder,
                            model_name = "GBQR",
                            specific_h = NULL) {
  
  ribbon_data <- purrr::map2_dfr(
    date_list,
    seq_along(date_list),
    function(d, i) {
      out_file <- file.path(
        "model_output",
        output_folder,
        paste0(d, "-", model_name, ".csv")
      )
      
      out <- read.csv(out_file)
      forecast_est_data(out, i)
    }
  )
  
  if (!is.null(specific_h)) {
    ribbon_data <- ribbon_data %>%
      filter(horizon == specific_h)
  }
  
  ribbon_data
}

df_obs_est <- function(ribbon_data, filtered_obs_data) {
  obs_inc <- filtered_obs_data %>%
    dplyr::select(target_end_date, location, inc) %>%
    mutate(
      target_end_date = as.Date(target_end_date),
      location = as.character(location)
    )
  
  obs_inc %>%
    left_join(
      ribbon_data %>%
        mutate(location = as.character(location)),
      by = c("location", "target_end_date")
    )
}

make_geo_mapping <- function(obs) {
  obs <- obs %>%
    mutate(
      hsa_nci_id = as.character(hsa_nci_id),
      cluster = as.character(cluster),
      cluster = ifelse(grepl("^G_", cluster), cluster, paste0("G_", cluster))
    )
  
  hsa_level <- obs %>%
    distinct(hsa_nci_id) %>%
    mutate(
      geo_level = "hsa",
      location = hsa_nci_id
    )
  
  cluster_level <- obs %>%
    distinct(hsa_nci_id, cluster) %>%
    mutate(
      geo_level = "cluster",
      location = cluster
    ) %>%
    dplyr::select(-cluster)
  
  state_level <- obs %>%
    distinct(hsa_nci_id) %>%
    mutate(
      geo_level = "state",
      location = "TX"
    )
  
  geo_mapping <- bind_rows(
    hsa_level,
    cluster_level,
    state_level
  )
  
  geo_wide_mapping <- state_level %>%
    mutate(state = location) %>%
    dplyr::select(hsa_nci_id, state) %>%
    left_join(
      cluster_level %>%
        mutate(cluster = location) %>%
        dplyr::select(hsa_nci_id, cluster),
      by = "hsa_nci_id"
    )
  
  list(
    geo_mapping = geo_mapping,
    geo_wide_mapping = geo_wide_mapping
  )
}

df_obs_est_wide <- function(df_all, geo_wide_mapping) {
  vars_to_keep <- c("inc", "est_low", "est_median", "est_high")
  
  df_hsa <- df_all %>%
    mutate(location = as.character(location)) %>%
    inner_join(
      geo_wide_mapping %>%
        mutate(location = as.character(hsa_nci_id)),
      by = "location"
    ) %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      hsa_nci_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_hsa"), all_of(vars_to_keep))
  
  df_cluster <- df_all %>%
    mutate(location = as.character(location)) %>%
    inner_join(
      geo_wide_mapping %>%
        mutate(location = as.character(cluster)),
      by = "location",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      hsa_nci_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_G"), all_of(vars_to_keep))
  
  df_state <- df_all %>%
    mutate(location = as.character(location)) %>%
    inner_join(
      geo_wide_mapping %>%
        mutate(location = as.character(state)),
      by = "location",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      hsa_nci_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_state"), all_of(vars_to_keep))
  
  df_hsa %>%
    left_join(
      df_cluster,
      by = c("target_end_date", "reference_date", "horizon", "hsa_nci_id")
    ) %>%
    left_join(
      df_state,
      by = c("target_end_date", "reference_date", "horizon", "hsa_nci_id")
    )
}

compute_wis_score <- function(df, predicted, observed, target_date) {
  df %>%
    rename(
      predicted = !!rlang::sym(predicted),
      observed = !!rlang::sym(observed),
      quantile_level = output_type_id
    ) %>%
    dplyr::select(
      reference_date,
      location,
      horizon,
      target,
      target_end_date,
      output_type,
      quantile_level,
      observed,
      predicted
    ) %>%
    as_forecast_quantile(
      forecast_unit = c("location", "target_end_date", "horizon", "target"),
      predicted = "predicted",
      quantile_col = "quantile_level"
    ) %>%
    score() %>%
    mutate(
      horizon = horizon + 1,
      reference_date = as.Date(target_date)
    ) %>%
    dplyr::select(location, target_end_date, horizon, reference_date, wis)
}

get_wis <- function(target_date,
                    output_folder,
                    model_name = "GBQR",
                    obs_all,
                    geo_mapping) {
  
  out_file <- file.path(
    "model_output",
    output_folder,
    paste0(target_date, "-", model_name, ".csv")
  )
  
  out <- read.csv(out_file)
  
  obs_all2 <- obs_all %>%
    mutate(
      target_end_date = as.Date(target_end_date),
      location = as.character(location)
    )
  
  out_all <- out %>%
    mutate(
      target_end_date = as.Date(target_end_date),
      location = as.character(location)
    ) %>%
    left_join(
      obs_all2,
      by = c("target_end_date", "location")
    ) %>%
    left_join(
      geo_mapping %>%
        dplyr::select(hsa_nci_id, location) %>%
        distinct(),
      by = "location",
      relationship = "many-to-many"
    )
  
  obs_hsa <- obs_all2 %>%
    filter(grepl("^[0-9]+$", location)) %>%
    dplyr::select(location, target_end_date, inc) %>%
    rename(inc_hsa = inc)
  
  out_all2 <- out_all %>%
    mutate(hsa_nci_id = as.character(hsa_nci_id)) %>%
    left_join(
      obs_hsa,
      by = c(
        "hsa_nci_id" = "location",
        "target_end_date"
      )
    ) %>%
    rename(original_location = location) %>%
    mutate(
      location = as.character(hsa_nci_id),
      forecast_level = case_when(
        grepl("^[0-9]+$", original_location) ~ "hsa",
        grepl("^G_", original_location) ~ "G",
        original_location == "TX" ~ "state",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(forecast_level))
  
  wis_same <- out_all2 %>%
    group_by(forecast_level) %>%
    group_map(
      ~ compute_wis_score(
        df = .x,
        predicted = "value",
        observed = "inc",
        target_date = target_date
      ) %>%
        mutate(wis_type = paste0("WIS_", .y$forecast_level)),
      .keep = TRUE
    ) %>%
    list_rbind()
  
  wis_vs_hsa <- out_all2 %>%
    filter(forecast_level %in% c("G", "state")) %>%
    group_by(forecast_level) %>%
    group_map(
      ~ compute_wis_score(
        df = .x,
        predicted = "value",
        observed = "inc_hsa",
        target_date = target_date
      ) %>%
        mutate(wis_type = paste0("WIS_", .y$forecast_level, "_vs_hsa")),
      .keep = TRUE
    ) %>%
    list_rbind()
  
  bind_rows(wis_same, wis_vs_hsa) %>%
    dplyr::select(
      location,
      target_end_date,
      horizon,
      reference_date,
      wis_type,
      wis
    ) %>%
    pivot_wider(
      names_from = wis_type,
      values_from = wis
    )
}

compute_wis_all_dates <- function(date_list,
                                  output_folder,
                                  model_name = "GBQR",
                                  obs_all,
                                  geo_mapping) {
  
  purrr::map_dfr(
    date_list,
    ~ get_wis(
      target_date = .x,
      output_folder = output_folder,
      model_name = model_name,
      obs_all = obs_all,
      geo_mapping = geo_mapping
    )
  )
}

add_eval_metrics <- function(df_all_wide) {
  df_all_wide %>%
    filter(!is.na(horizon)) %>%
    mutate(
      coverage_hsa = ifelse(inc_hsa >= est_low_hsa & inc_hsa <= est_high_hsa, 1, 0),
      MAE_hsa = abs(inc_hsa - est_median_hsa),
      
      coverage_state = ifelse(inc_state >= est_low_state & inc_state <= est_high_state, 1, 0),
      MAE_state = abs(inc_state - est_median_state),
      
      coverage_G = ifelse(inc_G >= est_low_G & inc_G <= est_high_G, 1, 0),
      MAE_G = abs(inc_G - est_median_G),
      
      coverage_state_vs_hsa = ifelse(inc_hsa >= est_low_state & inc_hsa <= est_high_state, 1, 0),
      MAE_state_vs_hsa = abs(inc_hsa - est_median_state),
      
      coverage_G_vs_hsa = ifelse(inc_hsa >= est_low_G & inc_hsa <= est_high_G, 1, 0),
      MAE_G_vs_hsa = abs(inc_hsa - est_median_G)
    )
}

summarize_metrics <- function(df_all_wide2,
                              method_name,
                              n_cluster) {
  df_all_wide2 %>%
    group_by(horizon) %>%
    summarise(
      coverage_hsa = mean(coverage_hsa, na.rm = TRUE),
      coverage_state = mean(coverage_state, na.rm = TRUE),
      coverage_G = mean(coverage_G, na.rm = TRUE),
      coverage_state_vs_hsa = mean(coverage_state_vs_hsa, na.rm = TRUE),
      coverage_G_vs_hsa = mean(coverage_G_vs_hsa, na.rm = TRUE),
      
      MAE_hsa = mean(MAE_hsa, na.rm = TRUE),
      MAE_state = mean(MAE_state, na.rm = TRUE),
      MAE_G = mean(MAE_G, na.rm = TRUE),
      MAE_state_vs_hsa = mean(MAE_state_vs_hsa, na.rm = TRUE),
      MAE_G_vs_hsa = mean(MAE_G_vs_hsa, na.rm = TRUE),
      
      WIS_hsa = mean(WIS_hsa, na.rm = TRUE),
      WIS_G = mean(WIS_G, na.rm = TRUE),
      WIS_state = mean(WIS_state, na.rm = TRUE),
      WIS_G_vs_hsa = mean(WIS_G_vs_hsa, na.rm = TRUE),
      WIS_state_vs_hsa = mean(WIS_state_vs_hsa, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      method_name = method_name,
      n_cluster = n_cluster
    )
}

make_horizon_plots <- function(df_all_wide,
                               date_list,
                               method_name,
                               n_cluster) {
  plot_list <- list()
  
  for (h in sort(unique(na.omit(df_all_wide$horizon)))) {
    plot_df_h <- df_all_wide %>%
      filter(horizon == h) %>%
      mutate(target_end_date = as.Date(target_end_date)) %>%
      filter(
        target_end_date >= as.Date(min(date_list) - 7),
        target_end_date <= as.Date(max(date_list) + 28)
      ) %>%
      pivot_longer(
        cols = c(
          est_median_hsa, est_median_G, est_median_state,
          est_low_hsa, est_low_G, est_low_state,
          est_high_hsa, est_high_G, est_high_state
        ),
        names_to = c(".value", "geo_level"),
        names_pattern = "est_(median|low|high)_(hsa|G|state)"
      ) %>%
      mutate(
        geo_level = factor(
          geo_level,
          levels = c("hsa", "G", "state"),
          labels = c("HSA", "Cluster", "State")
        )
      )
    
    point_df_h <- df_all_wide %>%
      filter(horizon == h) %>%
      mutate(target_end_date = as.Date(target_end_date)) %>%
      filter(
        target_end_date >= as.Date(min(date_list) - 7),
        target_end_date <= as.Date(max(date_list) + 28)
      )
    
    plot_list[[paste0("h", h)]] <- ggplot(plot_df_h, aes(x = target_end_date)) +
      geom_ribbon(aes(ymin = low, ymax = high, fill = geo_level), alpha = 0.15) +
      geom_line(aes(y = median, color = geo_level), linewidth = 0.8) +
      geom_point(
        data = point_df_h,
        aes(y = inc_hsa),
        color = "black",
        size = 0.8,
        alpha = 0.8
      ) +
      facet_wrap(~ hsa_nci_id, scales = "free_y") +
      labs(
        title = paste0(method_name, ", k = ", n_cluster, ", horizon = ", h),
        x = "Target end date",
        y = "% ED visits due to influenza",
        color = "Forecast level",
        fill = "Forecast level"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  plot_list
}

run_cluster_eval <- function(date_list,
                             n_cluster,
                             method_name,
                             model_name = "GBQR",
                             make_plots = TRUE) {
  
  obs <- read.csv(
    file.path("data/cluster_data", paste0("df_", method_name, "_", n_cluster, ".csv"))
  )
  
  obs_all <- read.csv(
    file.path("data/cluster_data", paste0("df_", method_name, "_", n_cluster, "_all.csv"))
  )
  
  output_folder <- paste0("TX_NSSP_", method_name, "_", n_cluster, "_pct")
  
  ribbon_data <- all_ribbon_data(
    date_list = date_list,
    output_folder = output_folder,
    model_name = model_name,
    specific_h = NULL
  )
  
  df_all <- df_obs_est(
    ribbon_data = ribbon_data,
    filtered_obs_data = obs_all
  )
  
  maps <- make_geo_mapping(obs)
  geo_mapping <- maps$geo_mapping
  geo_wide_mapping <- maps$geo_wide_mapping
  
  df_all_wide <- df_obs_est_wide(
    df_all = df_all,
    geo_wide_mapping = geo_wide_mapping
  )
  
  wis_all <- compute_wis_all_dates(
    date_list = date_list,
    output_folder = output_folder,
    model_name = model_name,
    obs_all = obs_all,
    geo_mapping = geo_mapping
  )
  
  df_all_wide <- df_all_wide %>%
    mutate(
      hsa_nci_id = as.character(hsa_nci_id),
      target_end_date = as.Date(target_end_date),
      reference_date = as.Date(reference_date)
    ) %>%
    left_join(
      wis_all %>%
        rename(hsa_nci_id = location) %>%
        mutate(
          hsa_nci_id = as.character(hsa_nci_id),
          target_end_date = as.Date(target_end_date),
          reference_date = as.Date(reference_date)
        ),
      by = c("target_end_date", "reference_date", "horizon", "hsa_nci_id")
    )
  
  df_all_wide2 <- add_eval_metrics(df_all_wide)
  
  summary_metrics <- summarize_metrics(
    df_all_wide2 = df_all_wide2,
    method_name = method_name,
    n_cluster = n_cluster
  )
  
  plot_list <- NULL
  if (make_plots) {
    plot_list <- make_horizon_plots(
      df_all_wide = df_all_wide,
      date_list = date_list,
      method_name = method_name,
      n_cluster = n_cluster
    )
  }
  
  list(
    method_name = method_name,
    n_cluster = n_cluster,
    obs = obs,
    obs_all = obs_all,
    df_all = df_all,
    df_all_wide = df_all_wide,
    df_all_wide2 = df_all_wide2,
    wis_all = wis_all,
    summary_metrics = summary_metrics,
    plots = plot_list
  )
}

make_summary_long <- function(summary_all) {
  summary_all %>%
    tidyr::pivot_longer(
      cols = c(
        coverage_hsa, coverage_state, coverage_G,
        coverage_state_vs_hsa, coverage_G_vs_hsa,
        MAE_hsa, MAE_state, MAE_G,
        MAE_state_vs_hsa, MAE_G_vs_hsa,
        WIS_hsa, WIS_G, WIS_state,
        WIS_G_vs_hsa, WIS_state_vs_hsa
      ),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_clean = dplyr::case_when(
        metric %in% c("coverage_hsa", "MAE_hsa", "WIS_hsa") ~ "HSA vs HSA",
        metric %in% c("coverage_G", "MAE_G", "WIS_G") ~ "Cluster vs Cluster",
        metric %in% c("coverage_state", "MAE_state", "WIS_state") ~ "State vs State",
        metric %in% c("coverage_G_vs_hsa", "MAE_G_vs_hsa", "WIS_G_vs_hsa") ~ "Cluster forecast vs HSA obs",
        metric %in% c("coverage_state_vs_hsa", "MAE_state_vs_hsa", "WIS_state_vs_hsa") ~ "State forecast vs HSA obs",
        TRUE ~ metric
      ),
      metric_clean = factor(
        metric_clean,
        levels = c(
          "HSA vs HSA",
          "Cluster vs Cluster",
          "State vs State",
          "Cluster forecast vs HSA obs",
          "State forecast vs HSA obs"
        )
      ),
      metric_type = dplyr::case_when(
        grepl("^coverage", metric) ~ "Coverage",
        grepl("^MAE", metric) ~ "MAE",
        grepl("^WIS", metric) ~ "WIS",
        TRUE ~ metric
      ),
      metric_type = factor(
        metric_type,
        levels = c("Coverage", "MAE", "WIS")
      )
    )
}


plot_summary_metrics <- function(summary_all, method_name = NULL) {
  
  summary_long2 <- make_summary_long(summary_all)
  
  p <- ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      metric_type ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Value",
      color = "Comparison",
      title = paste("Method:", method_name)
    )
  
  return(p)
}


plot_summary_method_type <- function(all_summary_long, metric_type_select) {
  
  all_summary_long2 <- all_summary_long %>%
    dplyr::filter(metric_type == metric_type_select)
  
  p <- ggplot2::ggplot(
    all_summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = method_name
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      metric_clean ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Value",
      color = "Method",
      title = paste("Metric Type:", metric_type_select)
    )
  
  return(p)
}


plot_fraction_better <- function(location_wis_all,
                                 unit_label = "HSA") {
  
  frac_better <- location_wis_all %>%
    dplyr::group_by(method_name, n_cluster, horizon) %>%
    dplyr::summarise(
      frac_cluster_better = mean(delta_wis_g_hsa < 0, na.rm = TRUE),
      frac_state_better   = mean(delta_wis_state_hsa < 0, na.rm = TRUE),
      n_locations = dplyr::n_distinct(location),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(frac_cluster_better, frac_state_better),
      names_to = "comparison",
      values_to = "fraction_better"
    ) %>%
    dplyr::mutate(
      comparison = dplyr::case_when(
        comparison == "frac_cluster_better" ~ paste0("Cluster better than ", unit_label),
        comparison == "frac_state_better"   ~ paste0("State better than ", unit_label),
        TRUE ~ comparison
      )
    )
  
  ggplot2::ggplot(
    frac_better,
    ggplot2::aes(
      x = n_cluster,
      y = fraction_better,
      color = comparison,
      group = comparison
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_grid(method_name ~ horizon) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = paste0("Fraction of ", unit_label, "s"),
      color = "Comparison",
      title = paste0("Fraction of ", unit_label, "s where aggregate forecast beats ", unit_label, " forecast")
    )
}
