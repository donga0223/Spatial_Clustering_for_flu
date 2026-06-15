source("code/create_forecasting_county_season_summary_rmd_functions.R")


tx_dshs <- read.csv("data/tx_dshs_region.csv")
tx_hsa <- read.csv("data/tx_hsa.csv")

# RAC 매핑 로드 (county level 분석에 필요)
rac_map <- read.csv("data/tx_rac.csv") %>%
  dplyr::select(RAC, County) %>%
  dplyr::rename(county = County) %>%
  dplyr::mutate(
    RAC    = as.character(RAC),
    county = as.character(county)
  ) %>%
  left_join(tx_dshs, by = c("county" = "County")) %>%
  left_join(tx_hsa, by = "county")


if(2==3){
  
  date_list2324 <- seq.Date(
    from = as.Date("2023-10-07"),
    to = as.Date("2023-10-15"),
    by = "week"
  )
  
  unit_id_var <- "county"
  unit_level_name <- "county"
  unit_label <- "County"
  
  res_clustergeo_5 <- run_cluster_eval(
    date_list = date_list2324,
    n_cluster = 5,
    method_name = "county_clustergeo",
    season = "2023-24",
    unit_id_var = "county",
    unit_level_name = "county",
    unit_label = "County",
    rac_map = rac_map,
    agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
    make_plots = TRUE
  )
  
  res_skater_k5$summary_metrics
  res_skater_k5$plots$h1
  
}

date_list_2324 <- seq.Date(
  from = as.Date("2023-10-07"),
  to = as.Date("2024-03-30"),
  by = "week"
)

date_list_2425 <- seq.Date(
  from = as.Date("2024-10-05"),
  to = as.Date("2025-03-30"),
  by = "week"
)

date_list_2526 <- seq.Date(
  from = as.Date("2025-10-04"),
  to = as.Date("2026-03-30"),
  by = "week"
)




methods <- c("county_clustergeo", "county_skater", "county_redcap")
seasons <- c("2023-24", "2024-25", "2025-26")

for (season in seasons) {
  
  # season에 맞는 date_list 설정
  if (season == "2023-24") {
    date_list <- date_list_2324
  } else if (season == "2024-25") {
    date_list <- date_list_2425
  } else if (season == "2025-26") {
    date_list <- date_list_2526
  }
  
  for (method_name in methods) {
    
    cat("\n=============================\n")
    cat("Season:", season, "\n")
    cat("Method:", method_name, "\n")
    cat("=============================\n")
    
    res_list <- list()
    
    for (k in 5:25) {
      
      cat("k =", k, "\n")
      
      res_list[[paste0("k", k)]] <- run_cluster_eval(
        date_list = date_list,
        n_cluster = k,
        method_name = method_name,
        season = season,
        unit_id_var = "county",
        unit_level_name = "county",
        unit_label = "County",
        rac_map = rac_map,
        agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
        make_plots = TRUE
      )
    }
    
    outfile <- file.path(
      "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results",
      paste0("summary_", method_name, "_", season, ".RData")
    )
    
    save(res_list, file = outfile)
    
    cat("Saved:", outfile, "\n")
  }
}

