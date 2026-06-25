# ==========================================================
# code/forecasting_trajectory_figure_function.R
# Forecast trajectory figure:
# observed unit-level incidence + forecast ribbons/medians
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

forecast_level_labels <- c(
  county = "County",
  hsa = "HSA",
  G = "Cluster",
  rac = "RAC",
  dshs_region = "DSHS Region",
  state = "State"
)

forecast_level_colors <- c(
  County = "#66a61e",
  HSA = "#e7298a",
  Cluster = "#7570b3",
  RAC = "#1b9e77",
  `DSHS Region` = "#d95f02",
  State = "#e6ab02"
)

make_forecast_horizon_plots <- function(df_all_wide,
                                        date_list,
                                        method_name,
                                        n_cluster,
                                        season = NULL,
                                        unit_level_name = "county",
                                        unit_label = "County",
                                        agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
                                        facet_var = "unit_id",
                                        unit_ids_select = NULL) {
  
  if (!is.null(unit_ids_select)) {
    df_all_wide <- df_all_wide %>%
      dplyr::filter(unit_id %in% unit_ids_select)
  }
  
  plot_list <- list()
  
  agg_levels <- agg_levels[agg_levels != unit_level_name]
  plot_levels <- c(unit_level_name, agg_levels)
  
  plot_levels <- plot_levels[
    paste0("est_median_", plot_levels) %in% names(df_all_wide) &
      paste0("est_low_", plot_levels) %in% names(df_all_wide) &
      paste0("est_high_", plot_levels) %in% names(df_all_wide)
  ]
  
  plot_cols <- unlist(lapply(
    c("est_median_", "est_low_", "est_high_"),
    function(prefix) paste0(prefix, plot_levels)
  ))
  
  level_labels <- forecast_level_labels
  level_labels[unit_level_name] <- unit_label
  
  inc_unit <- paste0("inc_", unit_level_name)
  
  if (!inc_unit %in% names(df_all_wide)) {
    stop("Missing observed unit column: ", inc_unit)
  }
  
  if (!facet_var %in% names(df_all_wide)) {
    stop("Missing facet column: ", facet_var)
  }
  
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
    
    title_text <- paste0(method_name, ", k = ", n_cluster, ", horizon = ", h)
    if (!is.null(season)) title_text <- paste0(title_text, ", season = ", season)
    
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
      facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y") +
      labs(
        title = title_text,
        x = "Target end date",
        y = "% ED visits due to influenza",
        color = "Forecast level",
        fill = "Forecast level"
      ) +
      scale_color_manual(values = forecast_level_colors, drop = FALSE) +
      scale_fill_manual(values = forecast_level_colors, drop = FALSE) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }
  
  plot_list
}