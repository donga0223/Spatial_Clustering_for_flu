library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)

source("code/spatial_variation/hsa_spatial_variation_functions.R")

# Data Loading and Mapping Preparation




#RUN_MODE = "single"
RUN_MODE = "all"

if (RUN_MODE == "single") {
  
  case <- load_spatial_variation_case(
    f_path = "data/cluster_data_season/df_hsa_redcap_exclude_2024-25_5_all.csv",
    dir_season = "data/cluster_data_season"
  )
  
  res_unweighted <- calculate_spatial_variation(
    obs_data = case$obs_test,
    mapping_df = case$mapping_table
  )
  
  res_weighted <- calculate_spatial_variation(
    obs_data = case$obs_test,
    mapping_df = case$mapping_table,
    weight_col = "population"
  )
  
  
} else {
  
  results_unweighted <- compile_spatial_variation_results(
    dir_season = "data/cluster_data_season",
    weight_col = NULL
  )
  
  results_weighted <- compile_spatial_variation_results(
    dir_season = "data/cluster_data_season",
    weight_col = "population"
  )
}


# ==========================================================
# Save / combine results
# ==========================================================

output_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (RUN_MODE == "single") {
  
  all_results_final <- dplyr::bind_rows(
    res_unweighted,
    res_weighted
  ) %>%
    dplyr::mutate(
      method = case$parsed$method,
      K = case$parsed$K,
      test_season = case$parsed$excluded_season,
      test_season_label = paste0("Test Season: ", case$parsed$excluded_season)
    )
  
} else {
  
  all_results_final <- dplyr::bind_rows(
    results_unweighted,
    results_weighted
  )
}

if (RUN_MODE == "all") {
  rds_path <- file.path(output_dir, "spatial_variation_results_hsa.rds")
  saveRDS(all_results_final, rds_path)
}
