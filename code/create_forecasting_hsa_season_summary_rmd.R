source("code/create_forecasting_county_season_summary_rmd_functions.R")

tx_hsa <- read.csv("data/tx_hsa.csv")

if(2==3){
  
  date_list2324 <- seq.Date(
    from = as.Date("2023-10-07"),
    to = as.Date("2023-10-15"),
    by = "week"
  )
  
  res_clustergeo_5 <- run_cluster_eval(
    date_list = date_list2324,
    n_cluster = 5,
    method_name = "hsa_clustergeo",
    season = "2023-24",
    unit_id_var = "hsa_nci_id",
    unit_level_name = "hsa",
    unit_label = "HSA",
    rac_map = tx_hsa,
    agg_levels = c("G", "state"),
    make_plots = TRUE
  )
  
  res_clustergeo_5$summary_metrics
  res_clustergeo_5$plots$h1
  
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




methods <- c("hsa_clustergeo", "hsa_skater", "hsa_redcap")
seasons <- c("2024-25", "2025-26")

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
    
    for (k in 2:22) {
      
      cat("k =", k, "\n")
      
      res_list[[paste0("k", k)]] <- run_cluster_eval(
        date_list = date_list,
        n_cluster = k,
        method_name = method_name,
        season = season,
        unit_id_var = "hsa_nci_id",
        unit_level_name = "hsa",
        unit_label = "HSA",
        rac_map = tx_hsa,
        agg_levels = c("G", "state"),
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

