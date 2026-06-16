

#load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo.RData")
source("code/forecasting_summary_figure_function.R")

load("/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/results/summary_county_clustergeo_2023-24.RData")
clustergeo_2324 <- res_list
rm(res_list)
load("/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/results/summary_county_clustergeo_2024-25.RData")
clustergeo_2425 <- res_list
rm(res_list)
load("/Users/dk29776/Dropbox/UTAustin/Spatial_clustering/results/summary_county_clustergeo_2025-26.RData")
clustergeo_2526 <- res_list
rm(res_list)

names(clustergeo_2324) <- paste0(names(clustergeo_2324), "_2324")
names(clustergeo_2425) <- paste0(names(clustergeo_2425), "_2425")
names(clustergeo_2526) <- paste0(names(clustergeo_2526), "_2526")

clustergeo_all <- c(clustergeo_2324, clustergeo_2425)

df_all_wide <- purrr::map_dfr(
  clustergeo_all,
  "df_all_wide2",
  .id = "result_id"
)

spatial_var_long <- purrr::map_dfr(
  clustergeo_all,
  "spatial_var_metrics",
  .id = "result_id"
) 


fig_2324 <- summary_figure(
  df_all_wide, 
  spatial_var_long, 
  "county_clustergeo", 
  season_select = "2324")
fig_2425 <- summary_figure(
  df_all_wide, 
  spatial_var_long, 
  "county_clustergeo", 
  season_select = "2425")
fig_overall <- summary_figure(
  df_all_wide, 
  spatial_var_long, 
  "county_clustergeo", 
  season_select = "overall")





summary_clustergeo_figure_2324 <- summary_figure(df_list = clustergeo_2324, 
                                                 method_name = "county_clustergeo", 
                                                 unit_level_name = "county", 
                                                 unit_label = "County")

summary_clustergeo_figure_2425 <- summary_figure(df_list = clustergeo_2425, 
                                                 method_name = "county_clustergeo", 
                                                 unit_level_name = "county", 
                                                 unit_label = "County")

summary_clustergeo_figure_2526 <- summary_figure(df_list = clustergeo_2526, 
                                                 method_name = "county_clustergeo", 
                                                 unit_level_name = "county", 
                                                 unit_label = "County")












