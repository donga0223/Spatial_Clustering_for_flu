#' Plot Spatial Variation Preserved for a Single Method with Full Points and Lines
#'
#' @param spatial_var Compiled results dataframe (all_results_final)
#' @param method_name Character string specifying the single method to plot 
#'                    (e.g., "skater", "redcap", or "clustergeo")
#' @param show_overall_only Logical. If TRUE, plots the 'Overall' average summary. 
#'                          If FALSE, facets by each test season row.
#'
plot_spatial_variation_custom <- function(spatial_var, 
                                          method_name, 
                                          show_overall_only = TRUE) {
  
  # 1. Enforce strict factor levels ordering (HSA -> Cluster -> RAC -> DSHS Region)
  level_order <- c(
    "County vs HSA",
    "County vs Cluster",
    "County vs RAC",
    "County vs DSHS Region"
  )
  
  color_order <- c(
    "HSA",
    "Cluster",
    "RAC",
    "DSHS Region"
  )
  
  # 2. Filter data based on the requested temporal scope (Overall vs Season-by-Season)
  if (show_overall_only) {
    spatial_filtered <- spatial_var |> dplyr::filter(test_season == "Overall")
  } else {
    spatial_filtered <- spatial_var |> dplyr::filter(test_season != "Overall")
  }
  
  # 3. Filter for fixed administrative boundaries OR the single specified clustering method
  spatial_filtered <- spatial_filtered |> 
    dplyr::filter(geo_level %in% c("rac", "dshs", "hsa") | (geo_level == "cluster" & method == method_name))
  
  # Error handling: Stop execution if the requested method string cannot be matched
  if (nrow(spatial_filtered) == 0) {
    stop(paste("❌ The specified method_name does not exist in the dataset:", method_name))
  }
  
  # 4. Standardize terminology and apply factor order configurations to align labels
  spatial_long <- spatial_filtered |>
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
  
  # 5. Aggregate metrics to calculate mean preserved variance per analytical unit
  spatial_summary <- spatial_long |>
    dplyr::group_by(K, metric_clean, color_group, period_type, test_season_label) |>
    dplyr::summarise(
      mean_preserved = mean(lambda_K, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 6. Build the ggplot grid framework using points and lines across all levels
  p <- ggplot2::ggplot(
    spatial_summary,
    ggplot2::aes(
      x = K,
      y = mean_preserved,
      color = color_group,
      group = metric_clean
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::ylim(0, NA) +
    ggplot2::theme_bw()
  
  # 7. Configure layout splitting based on temporal facet specifications
  if (show_overall_only) {
    p <- p + ggplot2::facet_wrap(~ period_type)
  } else {
    p <- p + ggplot2::facet_grid(test_season_label ~ period_type)
  }
  
  # 8. Bind formal title formatting and label systems
  p <- p + ggplot2::labs(
    x = "Number of clusters",
    y = "Spatial variation preserved",
    color = "Comparison",
    title = paste("Spatial variation preserved vs State - Method:", method_name)
  )
  
  # 9. Inject color palette specifications if global map vector is declared
  if (exists("geo_colors")) {
    p <- p + ggplot2::scale_color_manual(values = geo_colors, breaks = color_order)
  }
  
  return(p)
}


# Define the global color mapping dictionary matching the refined label layout keys
geo_colors <- c(
  "HSA"         = "#009E73",  # Green
  "Cluster"     = "#D55E00",  # Orange-Red (Highlights the selected clustering algorithm)
  "RAC"         = "#56B4E9",  # Light Blue
  "DSHS Region" = "#E69F00"   # Amber/Yellow
)

# Example 1: Generate Overall average plot for 'skater' method
p_skater_overall <- plot_spatial_variation_custom(
  spatial_var       = all_results_final, 
  method_name       = "skater",
  show_overall_only = TRUE
)
print(p_skater_overall)

# Example 2: Generate multi-tier cross-validation grid plot for 'redcap' method
p_redcap_seasons <- plot_spatial_variation_custom(
  spatial_var       = all_results_final, 
  method_name       = "redcap",
  show_overall_only = FALSE
)
print(p_redcap_seasons)



# Define output directory and file path
output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures"
pdf_path   <- file.path(output_dir, "spatial_variation_all_cases.pdf")

# Extract all unique clustering methods found in your data
available_methods <- unique(plot_cluster_data$method)

# Open a single PDF graphics device
pdf(file = pdf_path, width = 12, height = 8)

# Loop through each method and write pages sequentially
for (m in available_methods) {
  
  # Page 1 for Current Method: Overall Performance
  p_overall <- plot_spatial_variation_custom(
    spatial_var       = all_results_final, 
    method_name       = m, 
    show_overall_only = TRUE
  )
  print(p_overall) # Renders directly onto the current PDF page
  
  # Page 2 for Current Method: Season-by-Season Breakdown
  p_seasons <- plot_spatial_variation_custom(
    spatial_var       = all_results_final, 
    method_name       = m, 
    show_overall_only = FALSE
  )
  print(p_seasons) # Renders onto the next PDF page
}

# Close the file connection safely
dev.off()

message("Success! All cases compiled and saved to: ", pdf_path)
