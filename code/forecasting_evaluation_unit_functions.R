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
  
  unit_level <- obs %>%
    distinct(unit_id) %>%
    mutate(
      geo_level = unit_level_name,
      location = unit_id
    )
  
  cluster_level <- obs %>%
    distinct(unit_id, cluster) %>%
    mutate(
      geo_level = "cluster",
      location = cluster
    ) %>%
    select(-cluster)
  
  state_level <- obs %>%
    distinct(unit_id) %>%
    mutate(
      geo_level = "state",
      location = state_name
    )
  
  geo_mapping <- bind_rows(unit_level, cluster_level, state_level)
  
  geo_wide_mapping <- state_level %>%
    mutate(state = location) %>%
    select(unit_id, state) %>%
    left_join(
      cluster_level %>%
        mutate(cluster = location) %>%
        select(unit_id, cluster),
      by = "unit_id"
    )
  
  if (!is.null(rac_map) && unit_level_name == "county") {
    
    rac_map2 <- rac_map %>%
      mutate(
        county = as.character(county),
        hsa_nci_id = as.character(hsa_nci_id),
        RAC = as.character(RAC),
        DSHS_Region = as.character(DSHS_Region)
      )
    
    rac_level <- obs %>%
      distinct(unit_id) %>%
      mutate(county = unit_id) %>%
      left_join(rac_map2, by = "county") %>%
      filter(!is.na(RAC)) %>%
      mutate(
        geo_level = "rac",
        location = RAC
      ) %>%
      select(unit_id, geo_level, location)
    
    dshs_level <- obs %>%
      distinct(unit_id) %>%
      mutate(county = unit_id) %>%
      left_join(rac_map2, by = "county") %>%
      filter(!is.na(DSHS_Region)) %>%
      mutate(
        geo_level = "dshs_region",
        location = as.character(DSHS_Region)
      ) %>%
      select(unit_id, geo_level, location)
    
    hsa_level_from_county <- obs %>%
      distinct(unit_id) %>%
      mutate(county = unit_id) %>%
      left_join(rac_map2, by = "county") %>%
      filter(!is.na(hsa_nci_id)) %>%
      mutate(
        geo_level = "hsa",
        location = hsa_nci_id
      ) %>%
      select(unit_id, geo_level, location)
    
    geo_mapping <- bind_rows(
      geo_mapping,
      rac_level,
      dshs_level,
      hsa_level_from_county
    )
    
    geo_wide_mapping <- geo_wide_mapping %>%
      left_join(
        rac_level %>%
          mutate(rac = location) %>%
          select(unit_id, rac),
        by = "unit_id"
      ) %>%
      left_join(
        dshs_level %>%
          mutate(dshs_region = location) %>%
          select(unit_id, dshs_region),
        by = "unit_id"
      ) %>%
      left_join(
        hsa_level_from_county %>%
          mutate(hsa = location) %>%
          select(unit_id, hsa),
        by = "unit_id"
      )
  }
  
  list(
    geo_mapping = geo_mapping,
    geo_wide_mapping = geo_wide_mapping
  )
}


make_level_df <- function(df_all,
                          geo_wide_mapping,
                          level_col,
                          suffix,
                          vars_to_keep = c("inc", "est_low", "est_median", "est_high"),
                          relationship = NULL) {
  
  mapping <- geo_wide_mapping %>%
    mutate(location = as.character(.data[[level_col]])) %>%
    filter(!is.na(location))
  
  if (is.null(relationship)) {
    out <- df_all %>%
      mutate(location = as.character(location)) %>%
      inner_join(mapping, by = "location")
  } else {
    out <- df_all %>%
      mutate(location = as.character(location)) %>%
      inner_join(mapping,
                 by = "location",
                 relationship = relationship)
  }
  
  out %>%
    dplyr::select(
      target_end_date,
      reference_date,
      horizon,
      unit_id,
      all_of(vars_to_keep)
    ) %>%
    distinct() %>%
    rename_with(~ paste0(.x, "_", suffix), all_of(vars_to_keep))
}


df_obs_est_wide <- function(df_all,
                            geo_wide_mapping,
                            unit_level_name = "county") {
  
  vars_to_keep <- c("inc", "est_low", "est_median", "est_high")
  
  join_keys <- c(
    "target_end_date",
    "reference_date",
    "horizon",
    "unit_id"
  )
  
  df_unit <- make_level_df(
    df_all = df_all,
    geo_wide_mapping = geo_wide_mapping %>%
      mutate(unit_id = as.character(unit_id)),
    level_col = "unit_id",
    suffix = unit_level_name,
    vars_to_keep = vars_to_keep
  )
  
  df_cluster <- make_level_df(
    df_all,
    geo_wide_mapping,
    level_col = "cluster",
    suffix = "G",
    vars_to_keep = vars_to_keep,
    relationship = "many-to-many"
  )
  
  df_state <- make_level_df(
    df_all,
    geo_wide_mapping,
    level_col = "state",
    suffix = "state",
    vars_to_keep = vars_to_keep,
    relationship = "many-to-many"
  )
  
  out <- df_unit %>%
    left_join(df_cluster, by = join_keys) %>%
    left_join(df_state, by = join_keys)
  
  if ("rac" %in% names(geo_wide_mapping)) {
    df_rac <- make_level_df(
      df_all,
      geo_wide_mapping,
      level_col = "rac",
      suffix = "rac",
      vars_to_keep = vars_to_keep,
      relationship = "many-to-many"
    )
    
    out <- out %>%
      left_join(df_rac, by = join_keys)
  }
  
  if ("dshs_region" %in% names(geo_wide_mapping)) {
    df_dshs <- make_level_df(
      df_all,
      geo_wide_mapping,
      level_col = "dshs_region",
      suffix = "dshs_region",
      vars_to_keep = vars_to_keep,
      relationship = "many-to-many"
    )
    
    out <- out %>%
      left_join(df_dshs, by = join_keys)
  }
  
  if ("hsa" %in% names(geo_wide_mapping)) {
    df_hsa <- make_level_df(
      df_all,
      geo_wide_mapping,
      level_col = "hsa",
      suffix = "hsa",
      vars_to_keep = vars_to_keep,
      relationship = "many-to-many"
    )
    
    out <- out %>%
      left_join(df_hsa, by = join_keys)
  }
  
  out
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
  
  geo_mapping2 <- geo_mapping %>%
    mutate(
      unit_id = as.character(unit_id),
      location = as.character(location),
      geo_level = as.character(geo_level)
    ) %>%
    distinct()
  
  unit_locations <- geo_mapping2 %>%
    filter(geo_level == unit_level_name) %>%
    pull(location) %>%
    unique()
  
  unit_locations <- as.character(unit_locations)
  
  agg_levels <- geo_mapping2 %>%
    filter(!geo_level %in% unit_level_name) %>%
    pull(geo_level) %>%
    unique()
  
  agg_locations <- geo_mapping2 %>%
    filter(geo_level %in% agg_levels) %>%
    pull(location) %>%
    unique()
  
  out_all <- out %>%
    mutate(
      target_end_date = as.Date(target_end_date),
      location = as.character(location)
    ) %>%
    left_join(obs_all2, by = c("target_end_date", "location")) %>%
    left_join(
      geo_mapping2 %>%
        dplyr::select(unit_id, mapping_geo_level = geo_level, location) %>%
        distinct(),
      by = "location",
      relationship = "many-to-many"
    )
  
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
    rename(
      original_location = location,
      forecast_level_raw = mapping_geo_level
    ) %>%
    mutate(
      location = as.character(unit_id),
      forecast_level = case_when(
        forecast_level_raw == unit_level_name ~ unit_level_name,
        forecast_level_raw == "cluster" ~ "G",
        TRUE ~ forecast_level_raw
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
  
  vs_unit_levels <- out_all2 %>%
    filter(forecast_level != unit_level_name) %>%
    pull(forecast_level) %>%
    unique()
  
  wis_vs_unit <- out_all2 %>%
    filter(forecast_level %in% vs_unit_levels) %>%
    group_by(forecast_level) %>%
    group_map(
      ~ compute_wis_score(
        df = .x,
        predicted = "value",
        observed = "inc_unit",
        target_date = target_date
      ) %>%
        mutate(
          wis_type = paste0(
            "WIS_",
            .y$forecast_level,
            "_vs_",
            unit_level_name
          )
        ),
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
                             unit_level_name = "hsa",
                             agg_levels = c("G", "state", "rac", "dshs_region", "hsa")) {
  
  unit_suffix <- unit_level_name
  inc_unit <- paste0("inc_", unit_suffix)
  est_low_unit <- paste0("est_low_", unit_suffix)
  est_median_unit <- paste0("est_median_", unit_suffix)
  est_high_unit <- paste0("est_high_", unit_suffix)
  
  agg_levels <- agg_levels[
    agg_levels != unit_suffix &
      paste0("inc_", agg_levels) %in% names(df_all_wide) &
      paste0("est_low_", agg_levels) %in% names(df_all_wide) &
      paste0("est_median_", agg_levels) %in% names(df_all_wide) &
      paste0("est_high_", agg_levels) %in% names(df_all_wide)
  ]
  
  out <- df_all_wide %>%
    filter(!is.na(horizon)) %>%
    mutate(
      !!paste0("coverage_", unit_suffix) := ifelse(
        .data[[inc_unit]] >= .data[[est_low_unit]] &
          .data[[inc_unit]] <= .data[[est_high_unit]],
        1, 0
      ),
      !!paste0("MAE_", unit_suffix) := abs(
        .data[[inc_unit]] - .data[[est_median_unit]]
      )
    )
  
  for (lev in agg_levels) {
    
    inc_lev <- paste0("inc_", lev)
    est_low_lev <- paste0("est_low_", lev)
    est_median_lev <- paste0("est_median_", lev)
    est_high_lev <- paste0("est_high_", lev)
    
    out <- out %>%
      mutate(
        !!paste0("coverage_", lev) := ifelse(
          .data[[inc_lev]] >= .data[[est_low_lev]] &
            .data[[inc_lev]] <= .data[[est_high_lev]],
          1, 0
        ),
        
        !!paste0("MAE_", lev) := abs(
          .data[[inc_lev]] - .data[[est_median_lev]]
        ),
        
        !!paste0("coverage_", lev, "_vs_", unit_suffix) := ifelse(
          .data[[inc_unit]] >= .data[[est_low_lev]] &
            .data[[inc_unit]] <= .data[[est_high_lev]],
          1, 0
        ),
        
        !!paste0("MAE_", lev, "_vs_", unit_suffix) := abs(
          .data[[inc_unit]] - .data[[est_median_lev]]
        )
      )
  }
  
  out
}

add_spatial_variation_metric <- function(df_all_wide,
                                         summary_metrics,
                                         unit_level_name = "county",
                                         agg_levels = c("G", "rac", "dshs_region", "hsa"),
                                         state_level = "state") {
  
  inc_unit <- paste0("inc_", unit_level_name)
  inc_state <- paste0("inc_", state_level)
  
  agg_levels <- agg_levels[
    paste0("inc_", agg_levels) %in% names(df_all_wide)
  ]
  
  purrr::map_dfr(agg_levels, function(lev) {
    
    inc_lev <- paste0("inc_", lev)
    
    df_all_wide %>%
      filter(
        !is.na(.data[[inc_unit]]),
        !is.na(.data[[inc_lev]]),
        !is.na(.data[[inc_state]])
      ) %>%
      group_by(reference_date, target_end_date, horizon) %>%
      summarise(
        numerator = sum((.data[[inc_unit]] - .data[[inc_lev]])^2, na.rm = TRUE),
        denominator = sum((.data[[inc_unit]] - .data[[inc_state]])^2, na.rm = TRUE),
        spatial_variation_preserved = ifelse(
          denominator > 0,
          1 - numerator / denominator,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      mutate(
        geo_level = lev,
        metric = paste0("spatial_variation_", lev, "_vs_", state_level)
      )
  })
}

summarize_metrics <- function(df_all_wide2,
                              method_name,
                              n_cluster,
                              unit_level_name = "hsa",
                              agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  unit_metric_cols <- c(
    paste0("coverage_", unit_level_name),
    paste0("MAE_", unit_level_name),
    paste0("WIS_", unit_level_name)
  )
  
  for (lev in agg_levels) {
    if (lev == unit_level_name) next
    
    unit_metric_cols <- c(
      unit_metric_cols,
      paste0("coverage_", lev, "_vs_", unit_level_name),
      paste0("MAE_", lev, "_vs_", unit_level_name),
      paste0("WIS_", lev, "_vs_", unit_level_name),
      paste0("coverage_", lev),
      paste0("MAE_", lev),
      paste0("WIS_", lev)
    )
  }
  
  unit_metric_cols <- intersect(unit_metric_cols, names(df_all_wide2))
  
  summary_all <- df_all_wide2 %>%
    group_by(horizon) %>%
    summarise(
      across(all_of(unit_metric_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      method_name = method_name,
      n_cluster = n_cluster
    )
  
  summary_all
}

make_horizon_plots <- function(df_all_wide,
                               date_list,
                               method_name,
                               n_cluster,
                               unit_level_name = "hsa",
                               unit_label = "HSA",
                               agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  plot_list <- list()
  
  agg_levels <- agg_levels[agg_levels != unit_level_name]
  
  plot_levels <- c(unit_level_name, agg_levels)
  
  plot_levels <- plot_levels[
    paste0("est_median_", plot_levels) %in% names(df_all_wide) &
      paste0("est_low_", plot_levels) %in% names(df_all_wide) &
      paste0("est_high_", plot_levels) %in% names(df_all_wide)
  ]
  
  plot_cols <- unlist(
    lapply(
      c("est_median_", "est_low_", "est_high_"),
      function(prefix) paste0(prefix, plot_levels)
    )
  )
  
  level_labels <- c(
    hsa = "HSA",
    county = "County",
    G = "Cluster",
    rac = "RAC",
    dshs_region = "DSHS Region",
    state = "State"
  )
  
  level_labels[unit_level_name] <- unit_label
  
  inc_unit <- paste0("inc_", unit_level_name)
  
  for (h in sort(unique(na.omit(df_all_wide$horizon)))) {
    
    base_df_h <- df_all_wide %>%
      filter(horizon == h) %>%
      mutate(target_end_date = as.Date(target_end_date)) %>%
      filter(
        target_end_date >= as.Date(min(date_list) - 7),
        target_end_date <= as.Date(max(date_list) + 28)
      )
    
    plot_df_h <- base_df_h %>%
      pivot_longer(
        cols = all_of(plot_cols),
        names_to = c(".value", "geo_level"),
        names_pattern = "est_(median|low|high)_(.*)"
      ) %>%
      mutate(
        geo_level = factor(
          geo_level,
          levels = plot_levels,
          labels = unname(level_labels[plot_levels])
        )
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
        data = base_df_h,
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
                             agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
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
    obs = obs,
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
    unit_level_name = unit_level_name,
    agg_levels = agg_levels
  )
  
  spatial_agg_levels <- agg_levels
  spatial_agg_levels[spatial_agg_levels == "cluster"] <- "G"
  
  
  summary_metrics <- summarize_metrics(
    df_all_wide2 = df_all_wide2,
    method_name = method_name,
    n_cluster = n_cluster,
    unit_level_name = unit_level_name,
    agg_levels = agg_levels
  ) %>%
    left_join(spatial_var_summary, by = "horizon")
  
  spatial_var_metrics <- add_spatial_variation_metric(
    df_all_wide = df_all_wide2,
    unit_level_name = unit_level_name,
    agg_levels = spatial_agg_levels[spatial_agg_levels != "state"]
  )
  
  spatial_var_summary <- spatial_var_metrics %>%
    group_by(horizon, geo_level) %>%
    summarise(
      spatial_variation_preserved = mean(spatial_variation_preserved, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(metric_name = paste0("spatial_var_", geo_level)) %>%
    select(horizon, metric_name, spatial_variation_preserved) %>%
    pivot_wider(
      names_from = metric_name,
      values_from = spatial_variation_preserved
    )
  
  
  plot_list <- NULL
  
  if (make_plots) {
    plot_list <- make_horizon_plots(
      df_all_wide = df_all_wide,
      date_list = date_list,
      method_name = method_name,
      n_cluster = n_cluster,
      unit_level_name = unit_level_name,
      unit_label = unit_label,
      agg_levels = agg_levels
    )
  }
  
  list(
    method_name = method_name,
    n_cluster = n_cluster,
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    agg_levels = agg_levels,
    obs = obs,
    obs_all = obs_all,
    geo_mapping = geo_mapping,
    geo_wide_mapping = geo_wide_mapping,
    df_all = df_all,
    df_all_wide = df_all_wide,
    df_all_wide2 = df_all_wide2,
    wis_all = wis_all,
    spatial_var_metrics = spatial_var_metrics,
    summary_metrics = summary_metrics,
    plots = plot_list
  )
}

make_summary_long <- function(summary_all,
                              unit_level_name = "hsa",
                              unit_label = "HSA",
                              agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  agg_levels <- agg_levels[agg_levels != unit_level_name]
  
  level_labels <- c(
    G = "Cluster",
    rac = "RAC",
    dshs_region = "DSHS Region",
    hsa = "HSA",
    county = "County",
    state = "State"
  )
  
  level_labels[unit_level_name] <- unit_label
  
  metric_cols <- names(summary_all)[
    grepl("^(coverage|MAE|WIS|spatial_var)_", names(summary_all))
  ]
  
  summary_all %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::case_when(
        grepl("^coverage", metric) ~ "Coverage",
        grepl("^MAE", metric) ~ "MAE",
        grepl("^WIS", metric) ~ "WIS",
        grepl("^spatial_var", metric) ~ "Spatial variation",
        TRUE ~ metric
      ),
      
      level = dplyr::case_when(
        metric == paste0("coverage_", unit_level_name) |
          metric == paste0("MAE_", unit_level_name) |
          metric == paste0("WIS_", unit_level_name) ~ unit_level_name,
        
        grepl("_vs_", metric) ~ stringr::str_match(metric, "^(coverage|MAE|WIS)_(.*)_vs_")[, 3],
        
        grepl("^spatial_var_", metric) ~ stringr::str_remove(metric, "^spatial_var_"),
        
        TRUE ~ stringr::str_remove(metric, "^(coverage|MAE|WIS)_")
      ),
      
      metric_clean = dplyr::case_when(
        level == unit_level_name & !grepl("_vs_", metric) ~
          paste0(unit_label, " vs ", unit_label),
        
        grepl("_vs_", metric) ~
          paste0(level_labels[level], " forecast vs ", unit_label, " obs"),
        
        grepl("^spatial_var_", metric) ~
          paste0(level_labels[level], " spatial variation"),
        
        TRUE ~
          paste0(level_labels[level], " vs ", level_labels[level])
      ),
      
      metric_type = factor(
        metric_type,
        levels = c("Coverage", "MAE", "WIS", "Spatial variation")
      )
    )
}

plot_summary_metrics <- function(summary_all,
                                 method_name = NULL,
                                 unit_level_name = "hsa",
                                 unit_label = "HSA",
                                 agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  )
  
  p <- ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = metric_clean,
      group = metric_clean
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
      color = method_name,
      group = method_name
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