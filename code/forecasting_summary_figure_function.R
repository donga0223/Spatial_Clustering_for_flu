library(dplyr)
library(ggplot2)

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
      !stringr::str_detect(metric, paste0("_vs_", unit_level_name)),
      !stringr::str_detect(metric, "spatial_var")
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
      title = paste("Same-level evaluation - Method:", method_name)
    )
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
    ) |>
    dplyr::filter(
      !stringr::str_detect(metric, "spatial_var")
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
      title = paste("Evaluation against", unit_label, "observations - Method:", method_name)
    )
}

plot_spatial_variation_vs_state <- function(spatial_var,
                                            method_name = NULL) {
  
  level_order <- c(
    "County vs HSA",
    "County vs Cluster",
    "County vs RAC",
    "County vs DSHS Region"
  )
  
  spatial_long <- spatial_var |>
    dplyr::mutate(
      n_cluster = as.numeric(stringr::str_remove(result_id, "k")),
      metric_clean = dplyr::case_when(
        geo_level == "hsa" ~ "County vs HSA",
        geo_level == "G" ~ "County vs Cluster",
        geo_level == "rac" ~ "County vs RAC",
        geo_level == "dshs_region" ~ "County vs DSHS Region",
        TRUE ~ geo_level
      ),
      metric_clean = factor(metric_clean, levels = level_order)
    )
  
  spatial_summary <- spatial_long %>%
    group_by(n_cluster, metric_clean, horizon) %>%
    summarise(mean_preserved = mean(spatial_variation_preserved, na.rm = TRUE))
  
  ggplot2::ggplot(
    spatial_summary,
    ggplot2::aes(
      x = n_cluster,
      y = mean_preserved,
      color = metric_clean,
      group = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylim(0,NA) +
    ggplot2::facet_wrap(
      ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", method_name)
    )
}
