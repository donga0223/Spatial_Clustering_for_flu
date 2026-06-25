# ==========================================================
# run_forecasting_trajectory_figures.R
# ==========================================================

source("code/evaluation/forecasting_trajectory_figure_function.R")

base_dir <- "/work2/09967/dongahkim0223/frontera/Spatial_clustering"
result_dir <- file.path(base_dir, "results")
figure_dir <- file.path(base_dir, "figures")

load_result_list <- function(method,
                             season,
                             unit_level_name = "county",
                             result_dir_use = result_dir) {
  
  load(
    file.path(
      result_dir_use,
      paste0("summary_", unit_level_name, "_", method, "_", season, ".RData")
    )
  )
  
  res_list
}

#methods <- c("clustergeo", "skater", "redcap")
methods <- c("redcap")
seasons <- c("2023-24", "2024-25", "2025-26")

season_short <- c(
  "2023-24" = "2324",
  "2024-25" = "2425",
  "2025-26" = "2526"
)

date_lists <- list(
  "2023-24" = as.Date(c(
    "2023-10-07","2023-10-14","2023-10-21","2023-10-28",
    "2023-11-04","2023-11-11","2023-11-18","2023-11-25",
    "2023-12-02","2023-12-09","2023-12-16","2023-12-23","2023-12-30",
    "2024-01-06","2024-01-13","2024-01-20","2024-01-27",
    "2024-02-03","2024-02-10","2024-02-17","2024-02-24",
    "2024-03-02","2024-03-09","2024-03-16","2024-03-23","2024-03-30"
  )),
  "2024-25" = as.Date(c(
    "2024-10-05","2024-10-12","2024-10-19","2024-10-26",
    "2024-11-02","2024-11-09","2024-11-16","2024-11-23","2024-11-30",
    "2024-12-07","2024-12-14","2024-12-21","2024-12-28",
    "2025-01-04","2025-01-11","2025-01-18","2025-01-25",
    "2025-02-01","2025-02-08","2025-02-15","2025-02-22",
    "2025-03-01","2025-03-08","2025-03-15","2025-03-22","2025-03-29"
  )),
  "2025-26" = as.Date(c(
    "2025-10-04","2025-10-11","2025-10-18","2025-10-25",
    "2025-11-01","2025-11-08","2025-11-15","2025-11-22","2025-11-29",
    "2025-12-06","2025-12-13","2025-12-20","2025-12-27",
    "2026-01-03","2026-01-10","2026-01-17","2026-01-24","2026-01-31",
    "2026-02-07","2026-02-14","2026-02-21","2026-02-28",
    "2026-03-07","2026-03-14","2026-03-21","2026-03-28"
  ))
)

county_select <- c(
  "Anderson", "Bexar", "Brazoria", "Cameron", "Dallas",
  "El Paso", "Fort Bend", "Harris", "Hidalgo", "Jefferson",
  "Lubbock", "McLennan", "Midland", "Nueces", "Potter",
  "Tarrant", "Taylor", "Travis", "Webb", "Wichita"
)

trajectory_dir <- file.path(figure_dir, "forecast_trajectory")
dir.create(trajectory_dir, recursive = TRUE, showWarnings = FALSE)

for (method in methods) {
  message("\n==============================")
  message("METHOD: ", method)
  message("==============================")
  
  for (season in seasons) {
    message("\n--- SEASON: ", season, " ---")
    
    res_list <- load_result_list(
      method = method,
      season = season,
      unit_level_name = "county",
      result_dir_use = result_dir
    )
    
    for (nm in names(res_list)) {
      
      res <- res_list[[nm]]
      
      message(
        "Processing: method=", method,
        ", season=", season,
        ", k=", res$n_cluster
      )
      
      p_list <- make_forecast_horizon_plots(
        df_all_wide = res$df_all_wide,
        date_list = date_lists[[season]],
        method_name = res$method_name,
        n_cluster = res$n_cluster,
        season = season,
        unit_level_name = res$unit_level_name,
        unit_label = "County",
        agg_levels = res$agg_levels,
        facet_var = "unit_id",
        unit_ids_select = county_select
      )
      
      out_pdf <- file.path(
        trajectory_dir,
        paste0(
          "forecast_trajectory_county_",
          method, "_",
          season, "_k",
          res$n_cluster,
          ".pdf"
        )
      )
      
      pdf(out_pdf, width = 15, height = 10)
      for (p in p_list) print(p)
      dev.off()
    }
    
    rm(res_list)
  }
}
