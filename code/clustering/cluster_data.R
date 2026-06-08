## This script creates datasets for several methods with different numbers of clusters.
## Before running this script, you should first run `fPCA_contiguous.Rmd` or `fPCA_contiguous_county.Rmd`.

method_name <- "redcap"

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
    ) %>%
    dplyr::mutate(
      target_end_date = Date + 6
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







sf_county2 <- sf_county %>%
  dplyr::select(NAME, geometry) %>%
  rename(county = NAME)

mst_output <- make_spatial_mst(
  df_sf = sf_county, 
  data_matrix = scoring_matrix, 
  queen = FALSE
)

geo_distances <- make_clustgeo_distances(df_sf = sf_county2, data_matrix = scoring_matrix)

# 대역폭 가이드를 그리기 위해 원래 제공되는 함수도 바로 연결 가능합니다.
ClustGeo::choicealpha(geo_distances$D0, geo_distances$D1, range.alpha = seq(0, 1, 0.1), K = 7, graph = TRUE)

redcap_weights <- make_redcap_weights(df_sf = sf_county2, queen = FALSE)



method_name = "redcap"

for(i in 5:25){
  
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
  }else if(method_name == "clustergeo"){
    
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


    