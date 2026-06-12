source("code/forecasting_evaluation_unit_functions.R")

date_list2324 <- seq.Date(
  from = as.Date("2023-10-07"),
  to = as.Date("2024-03-30"),
  by = "week"
)

date_list2425 <- seq.Date(
  from = as.Date("2024-10-05"),
  to = as.Date("2025-03-30"),
  by = "week"
)

date_list2526 <- seq.Date(
  from = as.Date("2025-10-04"),
  to = as.Date("2026-03-30"),
  by = "week"
)

date_list = c(date_list2324, date_list2425, date_list2526)

unit_id_var <- "county"
unit_level_name <- "county"
unit_label <- "County"

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
  res_clustergeo_5 <- run_cluster_eval(
    date_list = date_list,
    n_cluster = 5,
    method_name = "county_clustergeo",
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





res_county_clustergeo_list <- list()

for (k in 5:25) {
  print(k)
  res_county_clustergeo_list[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_clustergeo",
    unit_id_var = "county",
    unit_level_name = "county",
    unit_label = "County",
    rac_map = rac_map,
    agg_levels = c("G", "rac", "dshs_region", "hsa", "state"),
    make_plots = TRUE
  )
}

save(res_county_clustergeo_list, 
     file = "/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo.RData")

