library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)

# Load the underlying metric calculator
source("code/calculate_spatial_variation_observed.R")

# ==============================================================================
# 1. Data Loading and Mapping Preparation
# ==============================================================================
rac_df    <- read.csv("data/tx_rac.csv") %>% rename(county = County)
dshs_meta <- read.csv("data/tx_dshs_region.csv")
hsa_meta  <- read.csv("data/tx_hsa.csv")

dir_season       <- "data/cluster_data_season"
file_list_season <- list.files(dir_season, pattern = "_all\\.csv$", full.names = TRUE)

# Run batch processing loop across all out-of-season model files
all_results <- purrr::map_dfr(file_list_season, function(f_path) {
  f_name <- basename(f_path)
  parsed_info <- stringr::str_match(f_name, "df_county_([a-zA-Z0-9]+)_exclude_([0-9]{4}-[0-9]{2})_([0-9]+)_all\\.csv")
  if (is.na(parsed_info[1])) return(NULL)
  
  curr_method  <- parsed_info[2]
  excluded_sea <- stringr::str_replace(parsed_info[3], "-", "/")
  curr_k_val   <- as.numeric(parsed_info[4])
  
  # Load data and immediately filter to retain ONLY the out-of-season target rows
  obs_all  <- read.csv(f_path)
  obs_test <- obs_all %>% dplyr::filter(season == excluded_sea)
  if (nrow(obs_test) == 0) return(NULL)
  
  meta_name <- stringr::str_replace(f_name, "_all\\.csv$", ".csv")
  meta_path <- file.path(dir_season, meta_name)
  if (!file.exists(meta_path)) return(NULL)
  
  cluster_tmp  <- read.csv(meta_path)
  cluster_meta <- cluster_tmp %>%
    dplyr::select(county, cluster) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cluster = paste0("G_", cluster))
  
  full_mapping_table <- rac_df %>%
    dplyr::select(county, rac = RAC) %>%
    dplyr::left_join(dshs_meta %>% dplyr::select(county, dshs = dshs_region), by = "county") %>%
    dplyr::left_join(hsa_meta  %>% dplyr::select(county, hsa_nci_id), by = "county") %>%
    dplyr::left_join(cluster_meta %>% dplyr::select(county, cluster), by = "county")
  
  calc <- function(peak_only) {
    calculate_spatial_variation_observed(
      obs_data = obs_test, mapping_df = full_mapping_table,
      agg_levels = c("rac", "dshs", "hsa", "cluster"),
      by_season = FALSE, compute_overall = TRUE, peak_weeks_only = peak_only,
      peak_months = c(10, 11, 12, 1, 2, 3)
    ) %>% dplyr::mutate(period_type = if (peak_only) "Peak Season Only" else "Full Period")
  }
  
  dplyr::bind_rows(calc(FALSE), calc(TRUE)) %>%
    dplyr::mutate(
      method = curr_method, K = curr_k_val, test_season = excluded_sea,
      test_season_label = paste0("Test Season: ", excluded_sea)
    )
})

# Compile Cross-Validation Overall averages
overall_results <- all_results %>%
  dplyr::group_by(method, K, geo_level, period_type) %>%
  dplyr::summarise(lambda_K = mean(lambda_K, na.rm = TRUE), n_weeks = sum(n_weeks, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(test_season = "Overall", test_season_label = "Overall")

all_results_final <- dplyr::bind_rows(all_results, overall_results)
output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results"
save_path_rds <- file.path(output_dir, "spatial_variation_season_results.rds")

# Save the single object safely
saveRDS(all_results_final, file = save_path_rds)
message("📂 Successfully saved data to: ", save_path_rds)

# ==============================================================================
# 2. Global Aesthetics Configuration (HSA -> Cluster -> RAC -> DSHS Region)
# ==============================================================================
# Define strict factor sequences matching your exact request
level_order <- c("County vs HSA", "County vs Cluster", "County vs RAC", "County vs DSHS Region")
color_order <- c("HSA", "Cluster", "RAC", "DSHS Region")

# Set up clean color palette mapping for the unified layout
geo_colors <- c(
  "HSA"         = "#009E73",  # Green
  "Cluster"     = "#D55E00",  # Orange/Red (Highlights the chosen method)
  "RAC"         = "#56B4E9",  # Light Blue
  "DSHS Region" = "#E69F00"   # Amber/Yellow
)

# Extract unique method labels present in the dataset dynamically
available_methods <- unique(all_results_final$method[all_results_final$geo_level == "cluster"])

# Define output file path details
output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures"
pdf_path   <- file.path(output_dir, "spatial_variation_all_cases.pdf")

# ==============================================================================
# 3. PDF Core Automated Engine Generation Execution
# ==============================================================================
# Initialize single multi-page PDF graphic device
pdf(file = pdf_path, width = 12, height = 8)

for (target_method in available_methods) {
  
  message("🖋️ Writing charts into PDF for method: ", target_method)
  
  # Filter and clean terminology for the current single method scope
  method_scoped_data <- all_results_final %>%
    dplyr::filter(geo_level %in% c("rac", "dshs", "hsa") | (geo_level == "cluster" & method == target_method)) %>%
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
  
  # ----------------------------------------------------------------------------
  # Page X: Plot Case 1 - Overall Average Summary (Full Period vs Peak Season)
  # ----------------------------------------------------------------------------
  p1_data <- method_scoped_data %>% dplyr::filter(test_season == "Overall")
  
  p1 <- ggplot2::ggplot(p1_data, ggplot2::aes(x = K, y = lambda_K, color = color_group, group = metric_clean)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_wrap(~ period_type, scales = "free_y") +
    ggplot2::scale_color_manual(values = geo_colors, breaks = color_order) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", target_method),
      subtitle = "Overall summary calculated across held-out test seasons"
    )
  
  # Print directly forces rendering onto the current active page inside the PDF 
  print(p1)
  
  # ----------------------------------------------------------------------------
  # Page X+1: Plot Case 2 - Multi-Tier Cross Validation Grid (By Individual Test Season)
  # ----------------------------------------------------------------------------
  p2_data <- method_scoped_data %>% dplyr::filter(test_season != "Overall")
  
  p2 <- ggplot2::ggplot(p2_data, ggplot2::aes(x = K, y = lambda_K, color = color_group, group = metric_clean)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_grid(test_season_label ~ period_type, scales = "free_y") +
    ggplot2::scale_color_manual(values = geo_colors, breaks = color_order) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", target_method),
      subtitle = "Each row shows Lambda computed on that specific season's held-out test data"
    )
  
  # Print forces rendering onto the subsequent page inside the PDF
  print(p2)
}

# Close the device connection cleanly to finalize the file write
dev.off()

message("🎉 Done! All combinations successfully exported as a single unified PDF file to:\n👉 ", pdf_path)
