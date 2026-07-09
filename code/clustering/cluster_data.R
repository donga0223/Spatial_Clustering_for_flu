## This script creates datasets for several methods with different numbers of clusters.
## Before running this script, you should first run `fPCA_contiguous.Rmd` or `fPCA_contiguous_county.Rmd`.

method_name <- "clustergeoaug"

for(i in 2:25){
  
  print(paste("Running", method_name, "k =", i))
  
  if(method_name == "clustergeo"){
    
    cluster_output <- run_clustgeo_cluster(
      df_sf         = sf_hsa,
      D0            = geo_distances$D0,
      D1            = geo_distances$D1,
      df_ts         = df_hsa,
      data_matrix   = scoring_matrix,
      alpha         = 0.2,               # data - spacious 
      n_clusters    = i,                 # selected # of cluster
      hsa_sf        = sf_hsa,
      sf_county     = NULL,  
      cities        = cities,            # 5 TX cities
      region_id_var = "hsa_nci_id",
      date_var      = "Date",
      num_var       = "hsa_value_flu",
      den_var       = "hsa_value_all"
    )
    
  } else if(method_name == "clustergeo0"){
    
    cluster_output <- run_clustgeo_cluster(
      df_sf         = sf_hsa,
      D0            = geo_distances$D0,
      D1            = geo_distances$D1,
      df_ts         = df_hsa,
      data_matrix   = scoring_matrix,
      alpha         = 0,               # 데이터-공간 융합 혼합 계수
      n_clusters    = i,                 # 분석가가 선택한 최종 K 개수
      hsa_sf        = sf_hsa,
      sf_county     = NULL,  
      cities        = cities,            # 데이터프레임 구조의 텍사스 대도시 데이터
      region_id_var = "hsa_nci_id",
      date_var      = "Date",
      num_var       = "hsa_value_flu",
      den_var       = "hsa_value_all"
    )
    } else if(method_name == "skater"){
    
    cluster_output <- run_skater_cluster(
      df_sf = sf_hsa,
      mst_res = mst_output$mst_res, 
      data_matrix = scoring_matrix,    
      df_ts = df_hsa,              
      n_clusters = i,               
      min_bound = 3,               
      hsa_sf = sf_hsa,
      cities = cities,           
      region_id_var = "hsa_nci_id",
      date_var = "Date",
      num_var = "hsa_value_flu",
      den_var = "hsa_value_all"
    )
    
  } else if(method_name == "redcap"){
    
    cluster_output <- run_redcap_cluster(
      df_sf         = sf_hsa,
      weights       = redcap_weights, 
      data_matrix   = scoring_matrix,
      df_ts         = df_hsa,
      n_clusters    = i,                 
      hsa_sf        = sf_hsa,
      cities        = cities,           
      region_id_var = "hsa_nci_id",
      date_var      = "Date",
      num_var       = "hsa_value_flu",
      den_var       = "hsa_value_all"
    )
    
  }
  
  df_final <- df_hsa %>%
    dplyr::left_join(
      cluster_output$cluster_mapping,
      by = "hsa_nci_id"
    ) 
  
  write.csv(
    df_final,
    paste0("data/cluster_data/df_", method_name, "_", i, ".csv"),
    row.names = FALSE
  )
  
  png_file <- paste0(
    "figures/cluster_combine/",
    method_name,
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


############################################################################################
## start from here
############################################################################################
df_long <- read.csv("data/county_edvisits.csv")
df_long <- df_long %>%
  filter(season != '2021/22')
sf_county <- readRDS("data/county_formap.RDS")
sf_hsa <- readRDS("data/hsa_formap.RDS")

df_long2 <- df_long %>%
  mutate(Date_parsed = as.Date(Date)) %>%
  filter(month(Date_parsed) %in% c(10, 11, 12, 1, 2, 3))

sf_county2 <- sf_county %>%
  dplyr::select(NAME, geometry) %>%
  rename(county = NAME)

make_county_scoring_matrix <- function(method_name) {
  if (method_name == "clustergeoaug") {
    get_augmented_clustering_features(
      df_ts = df_long2,
      group_var = "county",
      value_var = "value",
      den_var = "value_all",
      total_variance = 0.95,
      min_nharm = 10,
      fpca_weight = 1,
      seasonal_weight = 1,
      plotfit = FALSE
    )
  } else {
    get_pc_scores_seasonwise(
      df_ts = df_long2,
      group_var = county,
      total_variance = 0.95,
      min_nharm = 10,
      plotfit = FALSE
    )
  }
}

align_county_matrix_to_sf <- function(data_matrix, df_sf) {
  county_ids <- as.character(df_sf$county)
  matrix_ids <- as.character(rownames(data_matrix))
  missing_ids <- setdiff(county_ids, matrix_ids)
  
  if (length(missing_ids) > 0) {
    stop(
      "Feature matrix is missing counties from the county map: ",
      paste(missing_ids, collapse = ", ")
    )
  }
  
  data_matrix[county_ids, , drop = FALSE]
}

# Use whole data, restricted to flu-season months only.
# Set to "clustergeo" for FPCA-only ClustGeo, or "clustergeoaug" for
# augmented FPCA + seasonal-feature ClustGeo.
method_name <- "clustergeoaug"
k_values <- c(8, 15, 22, 30, 40, 50, 61, 65)

scoring_matrix <- make_county_scoring_matrix(method_name)
scoring_matrix <- align_county_matrix_to_sf(scoring_matrix, sf_county2)

mst_output <- make_spatial_mst(
  df_sf = sf_county2,
  data_matrix = scoring_matrix,
  queen = FALSE
)

geo_distances <- make_clustgeo_distances(df_sf = sf_county2, data_matrix = scoring_matrix)

# 대역폭 가이드를 그리기 위해 원래 제공되는 함수도 바로 연결 가능합니다.
ClustGeo::choicealpha(geo_distances$D0, geo_distances$D1, range.alpha = seq(0, 1, 0.1), K = 7, graph = TRUE)

redcap_weights <- make_redcap_weights(df_sf = sf_county2, queen = FALSE)

dir.create("data/cluster_data", showWarnings = FALSE, recursive = TRUE)
dir.create("figures/cluster_combine", showWarnings = FALSE, recursive = TRUE)

for(i in k_values){
  
  print(paste("Running", method_name, "k =", i))
  
  if(method_name == "skater"){
    cluster_output <- run_skater_cluster(
      df_sf = sf_county2,
      mst_res = mst_output$mst_res, 
      data_matrix = scoring_matrix,    
      df_ts = df_long,              
      n_clusters = i,               
      min_bound = 5,               
      hsa_sf = sf_hsa,
      cities = cities,           
      region_id_var = "county",
      date_var = "Date",
      num_var = "value_flu",
      den_var = "value_all"
    )
  }else if(method_name %in% c("clustergeo", "clustergeoaug")){
    
    cluster_output <- run_clustgeo_cluster(
      df_sf         = sf_county2,
      D0            = geo_distances$D0,
      D1            = geo_distances$D1,
      df_ts         = df_long,
      data_matrix   = scoring_matrix,
      alpha         = 0.2,               # 데이터-공간 융합 혼합 계수
      n_clusters    = i,                 # 분석가가 선택한 최종 K 개수
      hsa_sf        = sf_hsa,
      sf_county     = NULL,  
      cities        = cities,            # 데이터프레임 구조의 텍사스 대도시 데이터
      region_id_var = "county",
      date_var      = "Date",
      num_var       = "value_flu",
      den_var       = "value_all"
    )
  }else if(method_name == "clustergeo0"){
    
    cluster_output <- run_clustgeo_cluster(
      df_sf         = sf_county2,
      D0            = geo_distances$D0,
      D1            = geo_distances$D1,
      df_ts         = df_long,
      data_matrix   = scoring_matrix,
      alpha         = 0,               # 데이터-공간 융합 혼합 계수
      n_clusters    = i,                 # 분석가가 선택한 최종 K 개수
      hsa_sf        = sf_hsa,
      sf_county     = NULL,  
      cities        = cities,            # 데이터프레임 구조의 텍사스 대도시 데이터
      region_id_var = "county",
      date_var      = "Date",
      num_var       = "value_flu",
      den_var       = "value_all"
    )
  }else if(method_name == "redcap"){
    
    cluster_output <- run_redcap_cluster(
      df_sf         = sf_county2,
      weights       = redcap_weights, 
      data_matrix   = scoring_matrix,
      df_ts         = df_long,
      n_clusters    = i,                 
      hsa_sf        = sf_hsa,
      cities        = cities,           
      region_id_var = "county",
      date_var      = "Date",
      num_var       = "value_flu",
      den_var       = "value_all"
    )
  }
  
  df_final <- df_long %>%
    dplyr::left_join(
      cluster_output$cluster_mapping,
      by = "county"
    )
  
  write.csv(
    df_final,
    paste0("data/cluster_data/df_county_", method_name, "_", i, ".csv"),
    row.names = FALSE
  )
  
  png_file <- paste0(
    "figures/cluster_combine/county_",
    method_name,
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


    
