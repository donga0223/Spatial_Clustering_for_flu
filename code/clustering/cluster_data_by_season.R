library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
source("code/clustering/fPCA_contiguous_function.R")

get_env_value <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || value == "") {
    default
  } else {
    value
  }
}

parse_k_list <- function(env_name, default_values) {
  value <- Sys.getenv(env_name, unset = NA_character_)
  if (is.na(value) || value == "") {
    return(default_values)
  }
  
  out <- as.integer(trimws(unlist(strsplit(value, ","))))
  out <- out[!is.na(out)]
  
  if (length(out) == 0) {
    stop(env_name, " must contain at least one integer K value.")
  }
  
  sort(unique(out))
}

parse_csv_env <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || value == "") {
    return(default)
  }
  
  trimws(unlist(strsplit(value, ",")))
}

align_matrix_to_sf <- function(data_matrix, df_sf, id_col) {
  sf_ids <- as.character(df_sf[[id_col]])
  matrix_ids <- as.character(rownames(data_matrix))
  missing_ids <- setdiff(sf_ids, matrix_ids)
  
  if (length(missing_ids) > 0) {
    stop(
      "Feature matrix is missing IDs from spatial data: ",
      paste(missing_ids, collapse = ", ")
    )
  }
  
  data_matrix[sf_ids, , drop = FALSE]
}

method_name <- get_env_value("METHOD", "clustergeo")
output_method_name <- get_env_value("OUTPUT_METHOD", method_name)
run_levels <- parse_csv_env("RUN_LEVELS", c("county", "hsa"))
cluster_data_dir <- get_env_value("CLUSTER_DATA_DIR", "data/cluster_data_season")
cluster_figure_dir <- get_env_value("CLUSTER_FIGURE_DIR", "figures/cluster_combine")
clustgeo_alpha <- as.numeric(get_env_value("CLUSTGEO_ALPHA", "0.2"))
feature_set <- get_env_value("FEATURE_SET", "augmented")
fpca_weight <- as.numeric(get_env_value("FPCA_WEIGHT", "1"))
seasonal_feature_weight <- as.numeric(get_env_value("SEASONAL_FEATURE_WEIGHT", "1"))

if (!method_name %in% c("clustergeo", "skater", "redcap")) {
  stop("METHOD must be one of: clustergeo, skater, redcap.")
}

if (!grepl("^[A-Za-z0-9]+$", output_method_name)) {
  stop("OUTPUT_METHOD must use only letters and numbers so downstream filename parsing works.")
}

if (!all(run_levels %in% c("county", "hsa"))) {
  stop("RUN_LEVELS must contain only county and/or hsa.")
}

if (!feature_set %in% c("augmented", "fpca")) {
  stop("FEATURE_SET must be one of: augmented, fpca.")
}

if (is.na(clustgeo_alpha) || clustgeo_alpha < 0 || clustgeo_alpha > 1) {
  stop("CLUSTGEO_ALPHA must be a number between 0 and 1.")
}

if (is.na(fpca_weight) || fpca_weight < 0) {
  stop("FPCA_WEIGHT must be a non-negative number.")
}

if (is.na(seasonal_feature_weight) || seasonal_feature_weight < 0) {
  stop("SEASONAL_FEATURE_WEIGHT must be a non-negative number.")
}

county_k_values <- parse_k_list("COUNTY_K_LIST", seq(5, 65, 2))
hsa_k_values <- parse_k_list("HSA_K_LIST", 2:22)

message("Clustering method: ", method_name)
message("Output method label: ", output_method_name)
message("Run levels: ", paste(run_levels, collapse = ", "))
if (method_name == "clustergeo") {
  message("ClustGeo alpha: ", clustgeo_alpha)
}
message("Feature set: ", feature_set)
if (feature_set == "augmented") {
  message("FPCA weight: ", fpca_weight)
  message("Seasonal feature weight: ", seasonal_feature_weight)
}
message("County K values: ", paste(county_k_values, collapse = ", "))
message("HSA K values: ", paste(hsa_k_values, collapse = ", "))

dir.create(cluster_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cluster_figure_dir, showWarnings = FALSE, recursive = TRUE)

# Load raw datasets
df_long <- read.csv("data/county_edvisits.csv")
df_long <- df_long %>%
  filter(season != '2021/22')
sf_county <- readRDS("data/county_formap.RDS")
sf_hsa <- readRDS("data/hsa_formap.RDS")

# Define spatial coordinates for major Texas metropolitan areas
cities <- data.frame(
  name = c("Austin", "Houston", "Dallas", "El Paso", "San Antonio"),
  lon = c(-97.7431, -95.3698, -96.7970, -106.4850, -98.4936),
  lat = c(30.2672, 29.7604, 32.7767, 31.7619, 29.4241)
)

sf_county2 <- sf_county %>%
  dplyr::select(NAME, geometry) %>%
  dplyr::rename(county = NAME) %>%
  sf::st_as_sf(sf_column_name = "geometry")

# Extract unique seasons to iterate over as "Test Seasons"
#unique_seasons <- unique(df_long$season)
unique_seasons <- c("2023/24", "2024/25", "2025/26")

# =========================================================================
# [STEP 1] Outer Loop: Setting the Target 'Test' Season
# =========================================================================
if ("county" %in% run_levels) for (sea in unique_seasons) {
  
  # Replace slashes with hyphens to prevent file path errors during saving
  sea_safe <- gsub("/", "-", sea)
  print(paste("=================================================="))
  print(paste("Target TEST Season to Exclude:", sea))
  print(paste("=================================================="))
  
  # ★ CRITICAL: Exclude the target test season from clustering (Prevents Data Leakage)
  # This trains the cluster boundaries strictly on the remaining available seasons.
  df_train_seasons <- df_long %>% filter(season != sea)
  
  # This restricts the training data strictly to the peak influenza period (October to May (March)).
  df_train_in_season <- df_train_seasons %>%
    mutate(Date_parsed = as.Date(Date)) %>%
    filter(month(Date_parsed) %in% c(10, 11, 12, 1, 2, 3))
  
  # Compute Principal Component scores based on the combined historical training seasons
  #scoring_matrix <- get_pc_scores(
  #  df_ts = df_train_in_season,          
  #  group_var = county,     
  #  total_variance = 0.95, 
  #  min_nharm = 10, 
  #  plotfit = FALSE
  #)
  
  if (feature_set == "augmented") {
    scoring_matrix <- get_augmented_clustering_features(
      df_ts = df_train_in_season,
      group_var = "county",
      value_var = "value",
      den_var = "value_all",
      total_variance = 0.95,
      min_nharm = 10,
      fpca_weight = fpca_weight,
      seasonal_weight = seasonal_feature_weight,
      plotfit = FALSE
    )
  } else {
    scoring_matrix <- get_pc_scores_seasonwise(
      df_ts = df_train_in_season,
      group_var = county,
      total_variance = 0.95,
      min_nharm = 10, 
      plotfit = FALSE
    )
  }
  
  scoring_matrix <- align_matrix_to_sf(
    data_matrix = scoring_matrix,
    df_sf = sf_county2,
    id_col = "county"
  )
  
  # Construct a Spatial Minimum Spanning Tree (MST) using training data
  mst_output <- make_spatial_mst(
    df_sf = sf_county2, 
    data_matrix = scoring_matrix, 
    queen = FALSE
  )
  sf_county2 <- sf_county %>%
    dplyr::select(NAME, geometry) %>%
    dplyr::rename(county = NAME) %>%
    sf::st_as_sf(sf_column_name = "geometry")
  
  # Calculate geographic and data distance matrices for ClustGeo
  geo_distances <- make_clustgeo_distances(df_sf = sf_county2, data_matrix = scoring_matrix)
  redcap_weights <- make_redcap_weights(df_sf = sf_county2, queen = FALSE)
  
  # =========================================================================
  # [STEP 2] Inner Loop: Iterating over Cluster Scales (K = 5 to 25)
  # =========================================================================
  for(i in county_k_values){
    
    print(paste("Running", method_name, "| Output:", output_method_name, "| Excluded:", sea, "| K =", i))
    
    if(method_name == "skater"){
      cluster_output <- run_skater_cluster(
        df_sf = sf_county2,
        mst_res = mst_output$mst_res, 
        data_matrix = scoring_matrix,    
        df_ts = df_train_in_season,              
        n_clusters = i,               
        min_bound = 5,               
        hsa_sf = sf_hsa,
        cities = cities,           
        region_id_var = "county",
        date_var = "Date",
        num_var = "value_flu",
        den_var = "value_all"
      )
    } else if(method_name == "clustergeo"){
      cluster_output <- run_clustgeo_cluster(
        df_sf         = sf_county2,
        D0            = geo_distances$D0,
        D1            = geo_distances$D1,
        df_ts         = df_train_in_season,
        data_matrix   = scoring_matrix,
        alpha         = clustgeo_alpha,               
        n_clusters    = i,                 
        hsa_sf        = sf_hsa,
        sf_county     = NULL,  
        cities        = cities,            
        region_id_var = "county",
        date_var      = "Date",
        num_var       = "value_flu",
        den_var       = "value_all"
      )
    } else if(method_name == "redcap"){
      cluster_output <- run_redcap_cluster(
        df_sf         = sf_county2,
        weights       = redcap_weights, 
        data_matrix   = scoring_matrix,
        df_ts         = df_train_in_season,
        n_clusters    = i,                 
        hsa_sf        = sf_hsa,
        cities        = cities,           
        region_id_var = "county",
        date_var      = "Date",
        num_var       = "value_flu",
        den_var       = "value_all"
      )
    }
    
    # ★ NOTE: Join the cluster assignments back to the WHOLE dataset (or just the test season)
    # Here, we map it to the full df_long so that the test season also gets the cluster IDs assigned.
    df_final <- df_long %>%
      dplyr::left_join(
        cluster_output$cluster_mapping,
        by = "county"
      ) %>% 
      dplyr::mutate(
        target_end_date = as.Date(Date) + 6
      )
    
    p_map <- plot_cluster_map(
      hsa_sf2 = cluster_output$df_sf, 
      cluster_col = "cluster", 
      algo_name = method_name, 
      hsa_sf = sf_hsa, 
      sf_county = sf_county2,
      cities_sf = NULL
    )
    
    
    p_ts <- plot_cluster_trends(
      hsa_sf2 = cluster_output$df_sf,
      cluster_col = "cluster", 
      df_ts = df_long, 
      region_id_var = "county", 
      date_var ="Date", 
      num_var = "value_flu", 
      den_var = "value_all", 
      algo_name = method_name
    )
    
    p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
    
    # Save the mapped dataset. 
    # 'exclude_2021-22' implies this cluster model never saw the 2021/22 data during boundary generation.
    write.csv(
      df_final,
      file.path(
        cluster_data_dir,
        paste0("df_county_", output_method_name, "_exclude_", sea_safe, "_", i, ".csv")
      ),
      row.names = FALSE
    )
    
    # Export the combined diagnostic map
    png_file <- paste0(
      cluster_figure_dir,
      "/county_",
      output_method_name,
      ifelse(method_name == "clustergeo", paste0("_alpha", gsub("\\.", "p", clustgeo_alpha)), ""),
      "_exclude_",
      sea_safe,
      "_k",
      i,
      ".png"
    )
    ggplot2::ggsave(
      filename = png_file,
      plot = p_combined,
      width = 15,
      height = 10,
      dpi = 150
    )
  }
}

# =========================================================================
# =========================================================================
# Same steps for HSA level 
# =========================================================================
# =========================================================================

## need to change county to hsa and recalculate scoring_matrix and 
## mst_output by hsa level 
df_hsa <- df_long %>%
  group_by(season, Date, hsa_nci_id) %>%
  summarise(hsa_value_flu = sum(value_flu),
            hsa_value_all = sum(value_all),
            hsa_population = sum(population)) %>%
  ungroup() %>%
  mutate(value = if_else(hsa_value_flu == 0 | hsa_value_all == 0,
                         0, hsa_value_flu / hsa_value_all),
         Date = as.Date(Date),
         hsa_nci_id = as.integer(hsa_nci_id)) %>%
  dplyr::filter(!is.na(value))


# =========================================================================
# [STEP 1] Outer Loop: Setting the Target 'Test' Season
# =========================================================================
if ("hsa" %in% run_levels) for (sea in unique_seasons) {
  
  # Replace slashes with hyphens to prevent file path errors during saving
  sea_safe <- gsub("/", "-", sea)
  print(paste("=================================================="))
  print(paste("Target TEST Season to Exclude:", sea))
  print(paste("=================================================="))
  
  # ★ CRITICAL: Exclude the target test season from clustering (Prevents Data Leakage)
  # This trains the cluster boundaries strictly on the remaining available seasons.
  df_train_seasons <- df_hsa %>% filter(season != sea)
  
  # This restricts the training data strictly to the peak influenza period (October to May).
  df_train_in_season <- df_train_seasons %>%
    mutate(Date_parsed = as.Date(Date)) %>%
    filter(month(Date_parsed) %in% c(10, 11, 12, 1, 2, 3))
  
  # Compute Principal Component scores based on the combined historical training seasons
  if (feature_set == "augmented") {
    scoring_matrix <- get_augmented_clustering_features(
      df_ts = df_train_in_season,
      group_var = "hsa_nci_id",
      value_var = "value",
      den_var = "hsa_value_all",
      total_variance = 0.95,
      min_nharm = 10,
      fpca_weight = fpca_weight,
      seasonal_weight = seasonal_feature_weight,
      plotfit = FALSE
    )
  } else {
    scoring_matrix <- get_pc_scores_seasonwise(
      df_ts = df_train_in_season,          
      group_var = hsa_nci_id,     
      total_variance = 0.95,
      min_nharm = 10,
      plotfit = FALSE
    )
  }
  
  scoring_matrix <- align_matrix_to_sf(
    data_matrix = scoring_matrix,
    df_sf = sf_hsa,
    id_col = "hsa_nci_id"
  )
  
  # Construct a Spatial Minimum Spanning Tree (MST) using training data
  mst_output <- make_spatial_mst(
    df_sf = sf_hsa, 
    data_matrix = scoring_matrix, 
    queen = FALSE
  )

  
  # Calculate geographic and data distance matrices for ClustGeo
  geo_distances <- make_clustgeo_distances(df_sf = sf_hsa, data_matrix = scoring_matrix)
  redcap_weights <- make_redcap_weights(df_sf = sf_hsa, queen = FALSE)
  
  # =========================================================================
  # [STEP 2] Inner Loop: Iterating over Cluster Scales (K = 5 to 25)
  # =========================================================================
  for(i in hsa_k_values){
    
    print(paste("Running", method_name, "| Output:", output_method_name, "| Excluded:", sea, "| K =", i))
    
    if(method_name == "skater"){
      cluster_output <- run_skater_cluster(
        df_sf = sf_hsa,
        mst_res = mst_output$mst_res, 
        data_matrix = scoring_matrix,    
        df_ts = df_train_in_season,              
        n_clusters = i,               
        min_bound = 3,               
        hsa_sf = sf_hsa,
        cities = cities,           
        region_id_var = "hsa_nci_id",
        date_var = "Date",
        num_var = "hsa_value_flu",
        den_var = "hsa_value_all"
      )
    } else if(method_name == "clustergeo"){
      cluster_output <- run_clustgeo_cluster(
        df_sf         = sf_hsa,
        D0            = geo_distances$D0,
        D1            = geo_distances$D1,
        df_ts         = df_train_in_season,
        data_matrix   = scoring_matrix,
        alpha         = clustgeo_alpha,               
        n_clusters    = i,                 
        hsa_sf        = sf_hsa,
        sf_county     = NULL,  
        cities        = cities,            
        region_id_var = "hsa_nci_id",
        date_var      = "Date",
        num_var       = "hsa_value_flu",
        den_var       = "hsa_value_all"
      )
    } else if(method_name == "redcap"){
      cluster_output <- run_redcap_cluster(
        df_sf         = sf_hsa,
        weights       = redcap_weights, 
        data_matrix   = scoring_matrix,
        df_ts         = df_train_in_season,
        n_clusters    = i,                 
        hsa_sf        = sf_hsa,
        cities        = cities,           
        region_id_var = "hsa_nci_id",
        date_var      = "Date",
        num_var       = "hsa_value_flu",
        den_var       = "hsa_value_all"
      )
    }
    
    # ★ NOTE: Join the cluster assignments back to the WHOLE dataset (or just the test season)
    # Here, we map it to the full df_long so that the test season also gets the cluster IDs assigned.
    df_final <- df_hsa %>%
      dplyr::left_join(
        cluster_output$cluster_mapping,
        by = "hsa_nci_id"
      ) %>% 
      dplyr::mutate(
        target_end_date = as.Date(Date) + 6
      )
    
    p_map <- plot_cluster_map(
      hsa_sf2 = cluster_output$df_sf, 
      cluster_col = "cluster", 
      algo_name = method_name, 
      hsa_sf = sf_hsa, 
      sf_county = sf_county2,
      cities_sf = NULL
    )
    
    
    p_ts <- plot_cluster_trends(
      hsa_sf2 = cluster_output$df_sf,
      cluster_col = "cluster", 
      df_ts = df_hsa, 
      region_id_var = "hsa_nci_id", 
      date_var ="Date", 
      num_var = "hsa_value_flu", 
      den_var = "hsa_value_all", 
      algo_name = method_name
    )
    
    p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
    
    
    # Save the mapped dataset. 
    # 'exclude_2021-22' implies this cluster model never saw the 2021/22 data during boundary generation.
    write.csv(
      df_final,
      file.path(
        cluster_data_dir,
        paste0("df_hsa_", output_method_name, "_exclude_", sea_safe, "_", i, ".csv")
      ),
      row.names = FALSE
    )
    
    # Export the combined diagnostic map
    png_file <- paste0(
      cluster_figure_dir,
      "/hsa_",
      output_method_name,
      ifelse(method_name == "clustergeo", paste0("_alpha", gsub("\\.", "p", clustgeo_alpha)), ""),
      "_exclude_",
      sea_safe,
      "_k",
      i,
      ".png"
    )
    ggplot2::ggsave(
      filename = png_file,
      plot = cluster_output$p_combined,
      width = 15,
      height = 10,
      dpi = 150
    )
  }
}
