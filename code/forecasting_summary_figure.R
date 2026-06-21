source("code/forecasting_summary_figure_function.R")

# ==========================================================
# CLUSTERGEO
# ==========================================================
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo_2023-24.RData")
clustergeo_2324 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo_2024-25.RData")
clustergeo_2425 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo_2025-26.RData")
clustergeo_2526 <- res_list
rm(res_list)

names(clustergeo_2324) <- paste0(names(clustergeo_2324), "_2324")
names(clustergeo_2425) <- paste0(names(clustergeo_2425), "_2425")
names(clustergeo_2526) <- paste0(names(clustergeo_2526), "_2526")

clustergeo_all <- c(clustergeo_2324, clustergeo_2425, clustergeo_2526)

clustergeo_all_wide <- purrr::map_dfr(
  clustergeo_all,
  "df_all_wide2",
  .id = "result_id"
)

clustergeo_spatial_var_long <- purrr::map_dfr(
  clustergeo_all,
  "spatial_var_metrics",
  .id = "result_id"
) 


clustergeo_fig_2324 <- summary_figure(
  clustergeo_all_wide, 
  clustergeo_spatial_var_long, 
  "county_clustergeo", 
  season_select = "2324")
clustergeo_fig_2425 <- summary_figure(
  clustergeo_all_wide, 
  clustergeo_spatial_var_long, 
  "county_clustergeo", 
  season_select = "2425")
clustergeo_fig_2526 <- summary_figure(
  clustergeo_all_wide, 
  clustergeo_spatial_var_long, 
  "county_clustergeo", 
  season_select = "2526")
clustergeo_fig_overall <- summary_figure(
  clustergeo_all_wide, 
  clustergeo_spatial_var_long, 
  "county_clustergeo", 
  season_select = "overall")


pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary_clustergeo.pdf", width = 15, height = 10)
clustergeo_fig_2324$p1
clustergeo_fig_2324$p2
clustergeo_fig_2324$p_spatial
clustergeo_fig_2324$p_all

clustergeo_fig_2425$p1
clustergeo_fig_2425$p2
clustergeo_fig_2425$p_spatial
clustergeo_fig_2425$p_all

clustergeo_fig_2526$p1
clustergeo_fig_2526$p2
clustergeo_fig_2526$p_spatial
clustergeo_fig_2526$p_all

clustergeo_fig_overall$p1
clustergeo_fig_overall$p2
clustergeo_fig_overall$p_spatial
clustergeo_fig_overall$p_all
dev.off()

# ==========================================================
# ==========================================================
# SKATER
# ==========================================================
# ==========================================================
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_skater_2023-24.RData")
skater_2324 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_skater_2024-25.RData")
skater_2425 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_skater_2025-26.RData")
skater_2526 <- res_list
rm(res_list)

names(skater_2324) <- paste0(names(skater_2324), "_2324")
names(skater_2425) <- paste0(names(skater_2425), "_2425")
names(skater_2526) <- paste0(names(skater_2526), "_2526")

skater_all <- c(skater_2324, skater_2425, skater_2526)

skater_all_wide <- purrr::map_dfr(
  skater_all,
  "df_all_wide2",
  .id = "result_id"
)

skater_spatial_var_long <- purrr::map_dfr(
  skater_all,
  "spatial_var_metrics",
  .id = "result_id"
) 


skater_fig_2324 <- summary_figure(
  skater_all_wide, 
  skater_spatial_var_long, 
  "county_skater", 
  season_select = "2324")
skater_fig_2425 <- summary_figure(
  skater_all_wide, 
  skater_spatial_var_long, 
  "county_skater", 
  season_select = "2425")
skater_fig_2526 <- summary_figure(
  skater_all_wide, 
  skater_spatial_var_long, 
  "county_skater", 
  season_select = "2526")
skater_fig_overall <- summary_figure(
  skater_all_wide, 
  skater_spatial_var_long, 
  "county_skater", 
  season_select = "overall")

pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary_skater.pdf", width = 15, height = 10)
skater_fig_2324$p1
skater_fig_2324$p2
skater_fig_2324$p_spatial
skater_fig_2324$p_all

skater_fig_2425$p1
skater_fig_2425$p2
skater_fig_2425$p_spatial
skater_fig_2425$p_all

skater_fig_2526$p1
skater_fig_2526$p2
skater_fig_2526$p_spatial
skater_fig_2526$p_all

skater_fig_overall$p1
skater_fig_overall$p2
skater_fig_overall$p_spatial
skater_fig_overall$p_all
dev.off()


# ==========================================================
# ==========================================================
# redcap
# ==========================================================
# ==========================================================
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_redcap_2023-24.RData")
redcap_2324 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_redcap_2024-25.RData")
redcap_2425 <- res_list
rm(res_list)
load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_redcap_2025-26.RData")
redcap_2526 <- res_list
rm(res_list)

names(redcap_2324) <- paste0(names(redcap_2324), "_2324")
names(redcap_2425) <- paste0(names(redcap_2425), "_2425")
names(redcap_2526) <- paste0(names(redcap_2526), "_2526")

redcap_all <- c(redcap_2324, redcap_2425, redcap_2526)

redcap_all_wide <- purrr::map_dfr(
  redcap_all,
  "df_all_wide2",
  .id = "result_id"
)

redcap_spatial_var_long <- purrr::map_dfr(
  redcap_all,
  "spatial_var_metrics",
  .id = "result_id"
) 


redcap_fig_2324 <- summary_figure(
  redcap_all_wide, 
  redcap_spatial_var_long, 
  "county_redcap", 
  season_select = "2324")
redcap_fig_2425 <- summary_figure(
  redcap_all_wide, 
  redcap_spatial_var_long, 
  "county_redcap", 
  season_select = "2425")
redcap_fig_2526 <- summary_figure(
  redcap_all_wide, 
  redcap_spatial_var_long, 
  "county_redcap", 
  season_select = "2526")
redcap_fig_overall <- summary_figure(
  redcap_all_wide, 
  redcap_spatial_var_long, 
  "county_redcap", 
  season_select = "overall")


pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary_redcap.pdf", width = 15, height = 10)
redcap_fig_2324$p1
redcap_fig_2324$p2
redcap_fig_2324$p_spatial
redcap_fig_2324$p_all

redcap_fig_2425$p1
redcap_fig_2425$p2
redcap_fig_2425$p_spatial
redcap_fig_2425$p_all

redcap_fig_2526$p1
redcap_fig_2526$p2
redcap_fig_2526$p_spatial
redcap_fig_2526$p_all

redcap_fig_overall$p1
redcap_fig_overall$p2
redcap_fig_overall$p_spatial
redcap_fig_overall$p_all
dev.off()








