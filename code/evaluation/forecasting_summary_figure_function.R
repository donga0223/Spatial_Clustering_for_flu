library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

geo_colors <- c(
  "County"      = "#66a61e",
  "HSA"         = "#e7298a",
  "Cluster"     = "#7570b3",
  "RAC"         = "#1b9e77",
  "DSHS Region" = "#d95f02",
  "State"       = "#e6ab02"
)

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
    grepl("^(coverage|MAE|WIS)_", names(summary_all))
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
        TRUE ~ metric
      ),
      
      level = dplyr::case_when(
        metric == paste0("coverage_", unit_level_name) |
          metric == paste0("MAE_", unit_level_name) |
          metric == paste0("WIS_", unit_level_name) ~ unit_level_name,
        
        grepl("_vs_", metric) ~ stringr::str_match(metric, "^(coverage|MAE|WIS)_(.*)_vs_")[, 3],
        
        TRUE ~ stringr::str_remove(metric, "^(coverage|MAE|WIS)_")
      ),
      
      metric_clean = dplyr::case_when(
        level == unit_level_name & !grepl("_vs_", metric) ~
          paste0(unit_label, " vs ", unit_label),
        
        grepl("_vs_", metric) ~
          paste0(level_labels[level], " forecast vs ", unit_label, " obs"),
        
        TRUE ~
          paste0(level_labels[level], " vs ", level_labels[level])
      ),
      
      metric_type = factor(
        metric_type,
        levels = c("Coverage", "MAE", "WIS", "Spatial variation")
      ),
      color_group = dplyr::case_when(
        level == "county" ~ "County",
        level == "hsa" ~ "HSA",
        level == "G" ~ "Cluster",
        level == "rac" ~ "RAC",
        level == "dshs_region" ~ "DSHS Region",
        level == "state" ~ "State"
      ),
      color_group = factor(
        color_group,
        levels = c("County", "HSA", "Cluster", "RAC", "DSHS Region", "State")
      )
    )
}

plot_summary_metrics <- function(summary_all,
                                 method_name = NULL,
                                 unit_level_name = "county",
                                 unit_label = "County",
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
      color = color_group,
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
    ) +
    scale_color_manual(values = geo_colors)
  
  return(p)
}


plot_summary_metrics_same_level <- function(summary_all,
                                            method_name = NULL,
                                            unit_level_name = "county",
                                            unit_label = "County",
                                            agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  ) |>
    dplyr::filter(
      !stringr::str_detect(metric, paste0("_vs_", unit_level_name))
    )
  
  level_order_same <- c(
    "County vs County",
    "HSA vs HSA", 
    "Cluster vs Cluster",
    "RAC vs RAC",                
    "DSHS Region vs DSHS Region",
    "State vs State"   
  )
  
  summary_long2 <- summary_long2 |>
    dplyr::mutate(
      metric_clean = factor(
        metric_clean,
        levels = level_order_same
      )
    )
  ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = color_group,
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
      title = paste("Same-level evaluation - Method:", method_name)
    ) +
    scale_color_manual(values = geo_colors)
}

plot_summary_metrics_vs_unit <- function(summary_all,
                                         method_name = NULL,
                                         unit_level_name = "county",
                                         unit_label = "County",
                                         agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  ) |>
    dplyr::filter(
      stringr::str_detect(metric, paste0("_", unit_level_name, "$")) |
        stringr::str_detect(metric, paste0("_vs_", unit_level_name, "$"))
    )
  
  level_order_vs <- c(
    "County vs County",
    "HSA forecast vs County obs",
    "Cluster forecast vs County obs",
    "RAC forecast vs County obs",
    "DSHS Region forecast vs County obs",
    "State forecast vs County obs"      
  )
  summary_long2 <- summary_long2 |>
    dplyr::mutate(
      metric_clean = factor(
        metric_clean,
        levels = level_order_vs
      )
    )
  
  ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = color_group,
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
      title = paste("Evaluation against", unit_label, "observations - Method:", method_name)
    ) +
    scale_color_manual(values = geo_colors)
}

add_result_info <- function(df) {
  df %>%
    dplyr::mutate(
      n_cluster = as.numeric(stringr::str_extract(result_id, "(?<=k)\\d+")),
      season = stringr::str_extract(result_id, "\\d{4}$")
    )
}

summary_figure <- function(df_all_wide,
                           method_name,
                           unit_level_name = "county",
                           unit_label = "County",
                           agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
                           season_select = c("overall", "2324", "2425", "2526")) {
  
  season_select <- match.arg(season_select)
  
  df_all_wide <- add_result_info(df_all_wide)
  
  metric_cols <- names(df_all_wide)[
    grepl("^(coverage|MAE|WIS)_", names(df_all_wide))
  ]
  
  if (season_select == "overall") {
    
    df_summary <- df_all_wide %>%
      dplyr::group_by(n_cluster, horizon) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(metric_cols),
          ~ mean(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(method_name = method_name)
    
  } else {
    
    df_summary <- df_all_wide %>%
      dplyr::filter(season == season_select) %>%
      dplyr::group_by(n_cluster, horizon) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(metric_cols),
          ~ mean(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        method_name = method_name,
        season = season_select
      )
  }
  
  p1 <- plot_summary_metrics_same_level(
    summary_all = df_summary,
    method_name = paste(method_name, season_select),
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  )
  
  p2 <- plot_summary_metrics_vs_unit(
    summary_all = df_summary,
    method_name = paste(method_name, season_select),
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  )
  
  return(list(
    df_summary = df_summary,
    p1 = p1,
    p2 = p2
  ))
}


summarize_metrics <- function(df_all_wide,
                              method_name,
                              unit_level_name = "county",
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
  
  unit_metric_cols <- intersect(unit_metric_cols, names(df_all_wide))
  
  summary_all <- df_all_wide %>%
    mutate(
      n_cluster = as.numeric(stringr::str_extract(result_id, "(?<=k)\\d+")),
      season = stringr::str_extract(result_id, "\\d{4}$")
    ) %>%
    group_by(season, n_cluster, horizon) %>%
    summarise(
      across(all_of(unit_metric_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      method_name = method_name
    )
  
  summary_all
}

