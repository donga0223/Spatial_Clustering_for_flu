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

make_geo_mapping <- function(obs,
                             unit_id_var = "hsa_nci_id",
                             unit_level_name = "hsa",
                             cluster_prefix = "G_",
                             state_name = "TX",
                             rac_map = NULL) {
  
  obs <- obs %>%
    mutate(
      unit_id = as.character(.data[[unit_id_var]]),
      cluster = as.character(cluster),
      cluster = ifelse(
        grepl(paste0("^", cluster_prefix), cluster),
        cluster,
        paste0(cluster_prefix, cluster)
      )
    )
  
  # base geographic unit
  unit_level <- obs %>%
    distinct(unit_id) %>%
    mutate(
      geo_level = unit_level_name,
      location = unit_id
    )
  
  # cluster level
  cluster_level <- obs %>%
    distinct(unit_id, cluster) %>%
    mutate(
      geo_level = "cluster",
      location = cluster
    ) %>%
    dplyr::select(-cluster)
  
  # state level
  state_level <- obs %>%
    distinct(unit_id) %>%
    mutate(
      geo_level = "state",
      location = state_name
    )
  
  # RAC 레벨 추가 (county일 때만)
  if (!is.null(rac_map) && unit_level_name == "county") {
    rac_level <- obs %>%
      distinct(unit_id) %>%
      mutate(county = as.character(unit_id)) %>%
      left_join(rac_map %>% mutate(county = as.character(county)),
                by = "county") %>%
      filter(!is.na(RAC)) %>%
      mutate(geo_level = "rac", location = RAC) %>%
      dplyr::select(unit_id, geo_level, location)
    
    geo_mapping <- bind_rows(unit_level, cluster_level, state_level, rac_level)
  } else {
    geo_mapping <- bind_rows(unit_level, cluster_level, state_level)
  }
  
  geo_wide_mapping <- state_level %>%
    mutate(state = location) %>%
    dplyr::select(unit_id, state) %>%
    left_join(
      cluster_level %>%
        mutate(cluster = location) %>%
        dplyr::select(unit_id, cluster),
      by = "unit_id"
    )
  
  # AC wide mapping도 추가
  if (!is.null(rac_map) && unit_level_name == "county") {
    geo_wide_mapping <- geo_wide_mapping %>%
      left_join(
        rac_level %>% mutate(rac = location) %>% dplyr::select(unit_id, rac),
        by = "unit_id"
      )
  }
  
  list(
    geo_mapping = geo_mapping,
    geo_wide_mapping = geo_wide_mapping
  )
}


df_obs_est_wide <- function(df_all,
                            geo_wide_mapping,
                            unit_level_name = "hsa") {
  
  vars_to_keep <- c("inc", "est_low", "est_median", "est_high")
  
  df_unit <- df_all %>%
    mutate(location = as.character(location)) %>%
    inner_join(
      geo_wide_mapping %>%
        mutate(location = as.character(unit_id)),
      by = "location"
    ) %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      unit_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_", unit_level_name), all_of(vars_to_keep))
  
  df_rac <- df_all %>%
    mutate(location = as.character(location)) %>%
    inner_join(
      geo_wide_mapping %>%
        mutate(location = as.character(rac)),
      by = "location",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      unit_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_rac"), all_of(vars_to_keep))
  
  
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
      unit_id,
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
      unit_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_state"), all_of(vars_to_keep))
  
  df_unit %>%
    left_join(
      df_cluster,
      by = c("target_end_date", "reference_date", "horizon", "unit_id")
    ) %>%
    left_join(
      df_rac,
      by = c("target_end_date", "reference_date", "horizon", "unit_id")
    ) %>%
    left_join(
      df_state,
      by = c("target_end_date", "reference_date", "horizon", "unit_id")
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
                    geo_mapping,
                    unit_level_name = "hsa",
                    state_name = "TX",
                    cluster_prefix = "G_") {
  
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
  
  # base unit locations: hsa, county, rac, etc.
  unit_locations <- geo_mapping %>%
    filter(geo_level == unit_level_name) %>%
    pull(location) %>%
    as.character() %>%
    unique()
  
  # RAC locations 목록
  rac_locations <- geo_mapping %>%
    filter(geo_level == "rac") %>%
    pull(location) %>%
    as.character() %>%
    unique()
  
  has_rac <- length(rac_locations) > 0
  
  # unit/cluster/state 예측은 기존 방식대로 (many-to-many join으로 unit_id 확장)
  out_non_rac <- out %>%
    mutate(
      target_end_date = as.Date(target_end_date),
      location = as.character(location)
    ) %>%
    filter(!location %in% rac_locations) %>%
    left_join(obs_all2, by = c("target_end_date", "location")) %>%
    left_join(
      geo_mapping %>%
        filter(geo_level != "rac") %>%
        dplyr::select(unit_id, location) %>%
        distinct(),
      by = "location",
      relationship = "many-to-many"
    )
  
  # RAC 예측은 별도 처리: rac→county 매핑으로 unit_id 확장
  if (has_rac) {
    rac_to_unit <- geo_mapping %>%
      filter(geo_level == "rac") %>%
      dplyr::select(unit_id, location) %>%
      distinct()
    
    out_rac <- out %>%
      mutate(
        target_end_date = as.Date(target_end_date),
        location = as.character(location)
      ) %>%
      filter(location %in% rac_locations) %>%
      left_join(obs_all2, by = c("target_end_date", "location")) %>%
      left_join(rac_to_unit, by = "location", relationship = "many-to-many")
    
    out_all <- bind_rows(out_non_rac, out_rac)
  } else {
    out_all <- out_non_rac
  }

  obs_unit <- obs_all2 %>%
    filter(location %in% unit_locations) %>%
    dplyr::select(location, target_end_date, inc) %>%
    rename(inc_unit = inc)
  
  out_all2 <- out_all %>%
    mutate(unit_id = as.character(unit_id)) %>%
    left_join(
      obs_unit,
      by = c("unit_id" = "location", "target_end_date")
    ) %>%
    rename(original_location = location) %>%
    mutate(
      location = as.character(unit_id),
      forecast_level = case_when(
        original_location %in% unit_locations  ~ unit_level_name,
        grepl(paste0("^", cluster_prefix), original_location) ~ "G",
        original_location == state_name         ~ "state",
        original_location %in% rac_locations   ~ "rac",
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
  
  wis_vs_unit <- out_all2 %>%
    filter(forecast_level %in% c("G", "state", "rac")) %>%
    group_by(forecast_level) %>%
    group_map(
      ~ compute_wis_score(
        df = .x,
        predicted = "value",
        observed = "inc_unit",
        target_date = target_date
      ) %>%
        mutate(wis_type = paste0("WIS_", .y$forecast_level, "_vs_", unit_level_name)),
      .keep = TRUE
    ) %>%
    list_rbind()
  
  bind_rows(wis_same, wis_vs_unit) %>%
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
                                  geo_mapping,
                                  unit_level_name = "hsa",
                                  state_name = "TX",
                                  cluster_prefix = "G_") {
  
  purrr::map_dfr(
    date_list,
    ~ get_wis(
      target_date = .x,
      output_folder = output_folder,
      model_name = model_name,
      obs_all = obs_all,
      geo_mapping = geo_mapping,
      unit_level_name = unit_level_name,
      state_name = state_name,
      cluster_prefix = cluster_prefix
    )
  )
}

add_eval_metrics <- function(df_all_wide,
                             unit_level_name = "hsa") {
  
  inc_unit <- paste0("inc_", unit_level_name)
  est_low_unit <- paste0("est_low_", unit_level_name)
  est_median_unit <- paste0("est_median_", unit_level_name)
  est_high_unit <- paste0("est_high_", unit_level_name)
  
  coverage_unit <- paste0("coverage_", unit_level_name)
  MAE_unit <- paste0("MAE_", unit_level_name)
  
  coverage_state_vs_unit <- paste0("coverage_state_vs_", unit_level_name)
  MAE_state_vs_unit <- paste0("MAE_state_vs_", unit_level_name)
  
  coverage_G_vs_unit <- paste0("coverage_G_vs_", unit_level_name)
  MAE_G_vs_unit <- paste0("MAE_G_vs_", unit_level_name)
  
  coverage_rac_vs_unit <- paste0("coverage_rac_vs_", unit_level_name)
  MAE_rac_vs_unit <- paste0("MAE_rac_vs_", unit_level_name)
  
  df_all_wide %>%
    filter(!is.na(horizon)) %>%
    mutate(
      !!coverage_unit := ifelse(
        .data[[inc_unit]] >= .data[[est_low_unit]] &
          .data[[inc_unit]] <= .data[[est_high_unit]],
        1, 0
      ),
      
      !!MAE_unit := abs(.data[[inc_unit]] - .data[[est_median_unit]]),
      
      coverage_state = ifelse(
        inc_state >= est_low_state &
          inc_state <= est_high_state,
        1, 0
      ),
      
      MAE_state = abs(inc_state - est_median_state),
      
      coverage_G = ifelse(
        inc_G >= est_low_G &
          inc_G <= est_high_G,
        1, 0
      ),
      
      MAE_G = abs(inc_G - est_median_G),
      
      coverage_rac = ifelse(
        inc_rac >= est_low_rac &
          inc_rac <= est_high_rac,
        1, 0
      ),
      
      MAE_rac = abs(inc_rac - est_median_rac),
      
      !!coverage_state_vs_unit := ifelse(
        .data[[inc_unit]] >= est_low_state &
          .data[[inc_unit]] <= est_high_state,
        1, 0
      ),
      
      !!MAE_state_vs_unit := abs(
        .data[[inc_unit]] - est_median_state
      ),
      
      !!coverage_G_vs_unit := ifelse(
        .data[[inc_unit]] >= est_low_G &
          .data[[inc_unit]] <= est_high_G,
        1, 0
      ),
      
      !!MAE_G_vs_unit := abs(
        .data[[inc_unit]] - est_median_G
      ),
      
      !!coverage_rac_vs_unit := ifelse(
        .data[[inc_unit]] >= est_low_rac &
          .data[[inc_unit]] <= est_high_rac,
        1, 0
      ),
      
      !!MAE_rac_vs_unit := abs(
        .data[[inc_unit]] - est_median_rac
      )
    )
}

summarize_metrics <- function(df_all_wide2,
                              method_name,
                              n_cluster,
                              unit_level_name = "hsa") {
  
  unit_metric_cols <- c(
    paste0("coverage_", unit_level_name),
    paste0("coverage_state_vs_", unit_level_name),
    paste0("coverage_G_vs_", unit_level_name),
    paste0("coverage_rac_vs_", unit_level_name),
    
    paste0("MAE_", unit_level_name),
    paste0("MAE_state_vs_", unit_level_name),
    paste0("MAE_G_vs_", unit_level_name),
    paste0("MAE_rac_vs_", unit_level_name),
    
    paste0("WIS_", unit_level_name),
    paste0("WIS_G_vs_", unit_level_name),
    paste0("WIS_rac_vs_", unit_level_name),
    paste0("WIS_state_vs_", unit_level_name),
    
    "coverage_G", "MAE_G", "WIS_G",
    "coverage_rac", "MAE_rac", "WIS_rac"
  )
  
  state_metric_cols <- c("coverage_state", "MAE_state", "WIS_state")
  
  summary_unit <- df_all_wide2 %>%
    group_by(horizon) %>%
    summarise(
      across(all_of(unit_metric_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  summary_state <- df_all_wide2 %>%
    distinct(
      reference_date, target_end_date, horizon,
      coverage_state, MAE_state, WIS_state
    ) %>%
    group_by(horizon) %>%
    summarise(
      across(all_of(state_metric_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  summary_unit %>%
    left_join(summary_state, by = "horizon") %>%
    mutate(
      method_name = method_name,
      n_cluster = n_cluster
    )
}

make_horizon_plots <- function(df_all_wide,
                               date_list,
                               method_name,
                               n_cluster,
                               unit_level_name = "hsa",
                               unit_label = "HSA") {
  
  plot_list <- list()
  
  unit_cols <- paste0(
    c("est_median_", "est_low_", "est_high_"),
    unit_level_name
  )
  
  plot_cols <- c(
    unit_cols,
    "est_median_G", "est_median_rac", "est_median_state",
    "est_low_G",    "est_low_rac",    "est_low_state",
    "est_high_G",   "est_high_rac",   "est_high_state"
  )
  
  inc_unit <- paste0("inc_", unit_level_name)
  
  for (h in sort(unique(na.omit(df_all_wide$horizon)))) {
    
    plot_df_h <- df_all_wide %>%
      filter(horizon == h) %>%
      mutate(target_end_date = as.Date(target_end_date)) %>%
      filter(
        target_end_date >= as.Date(min(date_list) - 7),
        target_end_date <= as.Date(max(date_list) + 28)
      ) %>%
      pivot_longer(
        cols = all_of(plot_cols),
        names_to = c(".value", "geo_level"),
        names_pattern = paste0(
          "est_(median|low|high)_(",
          unit_level_name,
          "|G|rac|state)"
        )
      ) %>%
      mutate(
        geo_level = factor(
          geo_level,
          levels = c(unit_level_name, "G", "rac", "state"),
          labels = c(unit_label, "Cluster", "RAC", "State")
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
      geom_ribbon(
        aes(ymin = low, ymax = high, fill = geo_level),
        alpha = 0.15
      ) +
      geom_line(
        aes(y = median, color = geo_level),
        linewidth = 0.8
      ) +
      geom_point(
        data = point_df_h,
        aes(y = .data[[inc_unit]]),
        color = "black",
        size = 0.8,
        alpha = 0.8
      ) +
      facet_wrap(~ unit_id, scales = "free_y") +
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
                             unit_id_var = "hsa_nci_id",
                             unit_level_name = "hsa",
                             unit_label = NULL,
                             rac_map = NULL,
                             make_plots = TRUE) {
  
  if (is.null(unit_label)) {
    unit_label <- stringr::str_to_title(unit_level_name)
  }
  
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
  
  maps <- make_geo_mapping(
    obs,
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    rac_map = rac_map
  )
  
  geo_mapping <- maps$geo_mapping
  geo_wide_mapping <- maps$geo_wide_mapping
  
  df_all_wide <- df_obs_est_wide(
    df_all = df_all,
    geo_wide_mapping = geo_wide_mapping,
    unit_level_name = unit_level_name
  )
  
  wis_all <- compute_wis_all_dates(
    date_list = date_list,
    output_folder = output_folder,
    model_name = model_name,
    obs_all = obs_all,
    geo_mapping = geo_mapping,
    unit_level_name = unit_level_name
  )
  
  df_all_wide <- df_all_wide %>%
    mutate(
      unit_id = as.character(unit_id),
      target_end_date = as.Date(target_end_date),
      reference_date = as.Date(reference_date)
    ) %>%
    left_join(
      wis_all %>%
        rename(unit_id = location) %>%
        mutate(
          unit_id = as.character(unit_id),
          target_end_date = as.Date(target_end_date),
          reference_date = as.Date(reference_date)
        ),
      by = c("target_end_date", "reference_date", "horizon", "unit_id")
    )
  
  df_all_wide2 <- add_eval_metrics(
    df_all_wide,
    unit_level_name = unit_level_name
  )
  
  summary_metrics <- summarize_metrics(
    df_all_wide2 = df_all_wide2,
    method_name = method_name,
    n_cluster = n_cluster,
    unit_level_name = unit_level_name
  )
  
  plot_list <- NULL
  
  if (make_plots) {
    plot_list <- make_horizon_plots(
      df_all_wide = df_all_wide,
      date_list = date_list,
      method_name = method_name,
      n_cluster = n_cluster,
      unit_level_name = unit_level_name,
      unit_label = unit_label
    )
  }
  
  list(
    method_name = method_name,
    n_cluster = n_cluster,
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
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

make_summary_long <- function(summary_all,
                              unit_level_name = "hsa",
                              unit_label = "HSA") {
  
  unit_metrics <- c(
    paste0("coverage_", unit_level_name),
    paste0("MAE_", unit_level_name),
    paste0("WIS_", unit_level_name)
  )
  
  cluster_vs_unit_metrics <- c(
    paste0("coverage_G_vs_", unit_level_name),
    paste0("MAE_G_vs_", unit_level_name),
    paste0("WIS_G_vs_", unit_level_name)
  )
  
  state_vs_unit_metrics <- c(
    paste0("coverage_state_vs_", unit_level_name),
    paste0("MAE_state_vs_", unit_level_name),
    paste0("WIS_state_vs_", unit_level_name)
  )
  
  metric_cols <- c(
    unit_metrics,
    "coverage_state", "coverage_G",
    cluster_vs_unit_metrics,
    state_vs_unit_metrics,
    "MAE_state", "MAE_G",
    "WIS_G", "WIS_state"
  )
  
  summary_all %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_clean = dplyr::case_when(
        metric %in% unit_metrics ~ paste0(unit_label, " vs ", unit_label),
        metric %in% c("coverage_G", "MAE_G", "WIS_G") ~ "Cluster vs Cluster",
        metric %in% c("coverage_state", "MAE_state", "WIS_state") ~ "State vs State",
        metric %in% cluster_vs_unit_metrics ~ paste0("Cluster forecast vs ", unit_label, " obs"),
        metric %in% state_vs_unit_metrics ~ paste0("State forecast vs ", unit_label, " obs"),
        TRUE ~ metric
      ),
      metric_clean = factor(
        metric_clean,
        levels = c(
          paste0(unit_label, " vs ", unit_label),
          "Cluster vs Cluster",
          "State vs State",
          paste0("Cluster forecast vs ", unit_label, " obs"),
          paste0("State forecast vs ", unit_label, " obs")
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

plot_summary_metrics <- function(summary_all,
                                 method_name = NULL,
                                 unit_level_name = "hsa",
                                 unit_label = "HSA") {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label
  )
  
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


plot_summary_method_type <- function(all_summary_long,
                                     metric_type_select) {
  
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