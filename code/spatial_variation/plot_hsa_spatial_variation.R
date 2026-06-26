# ==========================================================
# Plot HSA spatial variation results
# ==========================================================

library(dplyr)
library(ggplot2)

results_path <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/spatial_variation_results_hsa.rds"

all_results_final <- readRDS(results_path)

output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/spatial_variation"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

plot_data <- all_results_final %>%
  dplyr::mutate(
    metric_clean = "HSA vs Cluster",
    color_group = "Cluster",
    metric_clean = factor(metric_clean, levels = "HSA vs Cluster"),
    color_group = factor(color_group, levels = "Cluster")
  )

available_methods <- unique(plot_data$method)

pdf_path <- file.path(output_dir, "spatial_variation_hsa_all_cases.pdf")

pdf(file = pdf_path, width = 8, height = 8)

for (target_method in available_methods) {
  
  message("Plotting method: ", target_method)
  
  method_scoped_data <- plot_data %>%
    dplyr::filter(method == target_method)
  
  # ========================================================
  # Overall plot
  # ========================================================
  p1_data <- method_scoped_data %>%
    dplyr::filter(test_season == "Overall")
  
  p1 <- ggplot2::ggplot(
    p1_data,
    ggplot2::aes(
      x = K,
      y = lambda_K,
      color = weight_type,
      group = weight_type
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_wrap(~ period_type, scales = "free_y") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Weight type",
      title = paste("HSA spatial variation preserved by cluster - Method:", target_method),
      subtitle = "Overall summary across held-out test seasons"
    )
  
  print(p1)
  
  # ========================================================
  # By held-out season plot
  # ========================================================
  p2_data <- method_scoped_data %>%
    dplyr::filter(test_season != "Overall")
  
  p2 <- ggplot2::ggplot(
    p2_data,
    ggplot2::aes(
      x = K,
      y = lambda_K,
      color = weight_type,
      group = weight_type
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_grid(test_season_label ~ period_type, scales = "free_y") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Weight type",
      title = paste("HSA spatial variation preserved by cluster - Method:", target_method),
      subtitle = "Each row shows lambda computed on each held-out test season"
    )
  
  print(p2)
}

dev.off()

message("Saved figure to: ", pdf_path)
