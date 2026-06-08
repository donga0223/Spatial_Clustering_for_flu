source("code/forecasting_evaluation_unit_functions.R")

date_list <- seq.Date(
  from = as.Date("2024-10-05"),
  to = as.Date("2025-03-01"),
  by = "week"
)

unit_id_var <- "county"
unit_level_name <- "county"
unit_label <- "County"

# RAC 매핑 로드 (county level 분석에 필요)
rac_map <- read.csv("data/tx_rac.csv") %>%
  dplyr::select(RAC, County) %>%
  dplyr::rename(county = County) %>%
  dplyr::mutate(
    RAC    = as.character(RAC),
    county = as.character(county)
  )


res_county_skater_list <- list()

for (k in 5:25) {
  print(k)
  res_county_skater_list[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_skater",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}


res_county_clustergeo_list <- list()

for (k in 5:25) {
  print(k)
  res_county_clustergeo_list[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_clustergeo",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}


res_county_redcap_list <- list()

for (k in 5:25) {
  print(k)
  res_county_redcap_list[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_redcap",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}

save(res_county_skater_list, 
     res_county_clustergeo_list, 
     res_county_redcap_list, 
     file = "wis_county_all.RData")

