library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)

source("code/calculate_spatial_variation_observed.R")
# ------------------------------------------------------------------------------
# 1. Define File Directory and Capture All Targets
# ------------------------------------------------------------------------------
file_path_list <- list.files("data/cluster_data", pattern = "_all\\.csv$", full.names = TRUE)

# ------------------------------------------------------------------------------
# 2. Main Loop for Processing Method / K / Period Combinations
# ------------------------------------------------------------------------------
all_results <- purrr::map_dfr(file_path_list, function(f_path) {
  
  # Extract Method and K from filename (e.g., "redcap_14" or "kmeans_5")
  f_name <- basename(f_path)
  parsed_info <- stringr::str_match(f_name, "df_county_([a-zA-Z0-9]+)_([0-9]+)_all\\.csv")
  
  # Skip file if it doesn't match the standard clustering naming design
  if (is.na(parsed_info[1])) return(NULL)
  
  curr_method  <- parsed_info[2]
  curr_k_val   <- as.numeric(parsed_info[3])
  
  # Load specific observation database for this cluster strategy
  obs_all <- read.csv(f_path)
  
  # Load matching metadata file to fetch county-cluster crosswalk mapping
  meta_path <- file.path("data/cluster_data", paste0("df_county_", curr_method, "_", curr_k_val, ".csv"))
  if (!file.exists(meta_path)) return(NULL)
  
  cluster_tmp <- read.csv(meta_path)
  cluster_meta <- cluster_tmp %>%
    dplyr::select(county, cluster) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cluster = paste0("G_", cluster))
  
  # Re-build consolidated master mapping table including current cluster mapping setup
  full_mapping_table <- rac_df %>% 
    dplyr::select(county, rac = RAC) %>% 
    dplyr::left_join(dshs_meta %>% dplyr::select(county, dshs = dshs_region), by = "county") %>% 
    dplyr::left_join(hsa_meta  %>% dplyr::select(county, hsa_nci_id), by = "county") %>%
    dplyr::left_join(cluster_meta %>% dplyr::select(county, cluster), by = "county") 
  
  # Run Variation Calculations for both Full Period and Peak Season
  m_normal <- calculate_spatial_variation_observed(
    obs_data = obs_all, mapping_df = full_mapping_table, 
    agg_levels = c("rac", "dshs", "hsa", "cluster"), peak_weeks_only = FALSE
  ) %>% dplyr::mutate(period_type = "Full Period")
  
  m_peak <- calculate_spatial_variation_observed(
    obs_data = obs_all, mapping_df = full_mapping_table, 
    agg_levels = c("rac", "dshs", "hsa", "cluster"), peak_weeks_only = TRUE,
    peak_months = c(10, 11, 12, 1, 2, 3)
  ) %>% dplyr::mutate(period_type = "Peak Season Only")
  
  # Combine results and inject specific Method and K features
  dplyr::bind_rows(m_normal, m_peak) %>%
    dplyr::mutate(
      method = curr_method,
      K = curr_k_val
    )
})


# ------------------------------------------------------------------------------
# 3. Data process for figures
# ------------------------------------------------------------------------------
# 1. Filter out cluster results (Lines that vary across K and Methods)
plot_cluster_data <- all_results %>% 
  dplyr::filter(geo_level == "cluster")

# 2. Extract baseline administrative levels (Fixed boundaries like RAC, DSHS, HSA)
# We average them to get a clean distinct reference value per season/period
plot_baseline_data <- all_results %>% 
  dplyr::filter(geo_level != "cluster") %>% 
  dplyr::group_by(geo_level, season, period_type) %>% 
  dplyr::summarise(lambda_K = mean(lambda_K), .groups = "drop")



# ------------------------------------------------------------------------------
# 4. Figures
# ------------------------------------------------------------------------------

ggplot() +
  # 1. Plot dynamic cluster method lines across varying K clusters
  geom_line(data = plot_cluster_data, 
            aes(x = K, y = lambda_K, color = method, group = method), 
            linewidth = 1) +
  geom_point(data = plot_cluster_data, 
             aes(x = K, y = lambda_K, color = method), 
             size = 2) +
  
  # 2. Overlay static reference boundaries as horizontal lines
  geom_hline(data = plot_baseline_data, 
             aes(yintercept = lambda_K, linetype = geo_level), 
             color = "gray40", alpha = 0.8) +
  
  # 3. Dynamic layout facet breakdown (Rows: Season / Columns: Period Type)
  facet_grid(season ~ period_type) +
  
  # 4. Themes and styling adjustments
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(color = "gray80", fill = NA),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = unique(plot_cluster_data$K)) + # Display exact K increments on X-axis
  labs(
    title = "Spatial Variation Preserved (Lambda K) by Clustering Strategy",
    subtitle = "Comparing dynamic clustering methods across cluster sizes (K) vs Fixed Administrative Levels",
    x = "Number of Clusters (K)",
    y = expression(bar(Lambda)[K]),
    color = "Clustering Method",
    linetype = "Baseline Geo Levels"
  )
