library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
source("code/fPCA_contiguous_function.R")

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
  rename(county = NAME)

# Extract unique seasons to iterate over as "Test Seasons"
unique_seasons <- unique(df_long$season)
#unique_seasons <- c("2022/23", "2023/24", "2024/25", "2025/26")
method_name <- "clustergeo"

# =========================================================================
# [STEP 1] Outer Loop: Setting the Target 'Test' Season
# =========================================================================
for (sea in unique_seasons) {
  
  # Replace slashes with hyphens to prevent file path errors during saving
  sea_safe <- gsub("/", "-", sea)
  print(paste("=================================================="))
  print(paste("Target TEST Season to Exclude:", sea))
  print(paste("=================================================="))
  
  # ★ CRITICAL: Exclude the target test season from clustering (Prevents Data Leakage)
  # This trains the cluster boundaries strictly on the remaining available seasons.
  df_train_seasons <- df_long %>% filter(season != sea)
  
  # This restricts the training data strictly to the peak influenza period (October to May).
  df_train_in_season <- df_train_seasons %>%
    mutate(Date_parsed = as.Date(Date)) %>%
    filter(month(Date_parsed) %in% c(10, 11, 12, 1, 2, 3, 4, 5))
  
  # Compute Principal Component scores based on the combined historical training seasons
  scoring_matrix <- get_pc_scores(
    df_ts = df_train_in_season,          
    group_var = county,     
    total_variance = 0.95,   
    plotfit = FALSE
  )
  
  # Construct a Spatial Minimum Spanning Tree (MST) using training data
  mst_output <- make_spatial_mst(
    df_sf = sf_county, 
    data_matrix = scoring_matrix, 
    queen = FALSE
  )
  sf_county2 <- sf_county %>%
    dplyr::select(NAME, geometry) %>%
    rename(county = NAME)
  
  # Calculate geographic and data distance matrices for ClustGeo
  geo_distances <- make_clustgeo_distances(df_sf = sf_county2, data_matrix = scoring_matrix)
  redcap_weights <- make_redcap_weights(df_sf = sf_county2, queen = FALSE)
  
  # =========================================================================
  # [STEP 2] Inner Loop: Iterating over Cluster Scales (K = 5 to 25)
  # =========================================================================
  for(i in 5:25){
    
    print(paste("Running", method_name, "| Excluded:", sea, "| K =", i))
    
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
        alpha         = 0.2,               
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
    
    # Save the mapped dataset. 
    # 'exclude_2021-22' implies this cluster model never saw the 2021/22 data during boundary generation.
    write.csv(
      df_final,
      paste0("data/cluster_data_season/df_county_", method_name, "_exclude_", sea_safe, "_", i, ".csv"),
      row.names = FALSE
    )
    
    # Export the combined diagnostic map
    png_file <- paste0(
      "/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/cluster_combine/county_",
      method_name,
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
