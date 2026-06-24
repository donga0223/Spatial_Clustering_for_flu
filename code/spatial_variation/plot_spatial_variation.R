
# ==========================================================
# Plot spatial variation results
# ==========================================================

library(dplyr)
library(ggplot2)

results_path <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/spatial_variation_results.rds"

all_results_final <- readRDS(results_path)

output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/spatial_variation"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

level_order <- c(
  "County vs HSA",
  "County vs Cluster",
  "County vs RAC",
  "County vs DSHS Region"
)

color_order <- c("HSA", "Cluster", "RAC", "DSHS Region")

geo_colors <- c(
  "HSA"         = "#009E73",
  "Cluster"     = "#D55E00",
  "RAC"         = "#56B4E9",
  "DSHS Region" = "#E69F00"
)

plot_data <- all_results_final %>%
  dplyr::mutate(
    metric_clean = dplyr::case_when(
      geo_level == "hsa"     ~ "County vs HSA",
      geo_level == "cluster" ~ "County vs Cluster",
      geo_level == "rac"     ~ "County vs RAC",
      geo_level == "dshs"    ~ "County vs DSHS Region",
      TRUE ~ geo_level
    ),
    color_group = dplyr::case_when(
      geo_level == "hsa"     ~ "HSA",
      geo_level == "cluster" ~ "Cluster",
      geo_level == "rac"     ~ "RAC",
      geo_level == "dshs"    ~ "DSHS Region",
      TRUE ~ geo_level
    ),
    metric_clean = factor(metric_clean, levels = level_order),
    color_group  = factor(color_group, levels = color_order)
  )

available_methods <- unique(plot_data$method[plot_data$geo_level == "cluster"])

pdf_path <- file.path(output_dir, "spatial_variation_all_cases.pdf")

pdf(file = pdf_path, width = 8, height = 12)

for (target_method in available_methods) {
  
  message("Plotting method: ", target_method)
  
  method_scoped_data <- plot_data %>%
    dplyr::filter(
      geo_level %in% c("rac", "dshs", "hsa") |
        (geo_level == "cluster" & method == target_method)
    )
  
  p1_data <- method_scoped_data %>%
    dplyr::filter(test_season == "Overall")
  
  p1 <- ggplot2::ggplot(
    p1_data,
    ggplot2::aes(
      x = K,
      y = lambda_K,
      color = color_group,
      group = interaction(metric_clean, weight_type)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_grid(weight_type ~ period_type, scales = "free_y") +
    ggplot2::scale_color_manual(values = geo_colors, breaks = color_order) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", target_method),
      subtitle = "Overall summary across held-out test seasons"
    )
  
  print(p1)
  
  p2_data <- method_scoped_data %>%
    dplyr::filter(test_season != "Overall")
  
  p2 <- ggplot2::ggplot(
    p2_data,
    ggplot2::aes(
      x = K,
      y = lambda_K,
      color = color_group,
      group = metric_clean
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_grid(test_season_label + weight_type ~ period_type, scales = "free_y") +
    ggplot2::scale_color_manual(values = geo_colors, breaks = color_order) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", target_method),
      subtitle = "Each row shows lambda computed on each held-out test season"
    )
  
  print(p2)
}

dev.off()

message("Saved figure to: ", pdf_path)

