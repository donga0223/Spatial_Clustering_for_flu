# ==========================================================
# run_forecasting_summary_figures.R
# ==========================================================

source("code/evaluation/forecasting_summary_figure_function.R")

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

make_method_summary_pdf <- function(method,
                                    unit_level_name = "county",
                                    unit_label = "County") {
  
  message("\n==============================")
  message("METHOD: ", method)
  message("==============================")
  
  message("Loading result files...")
  
  res_2324 <- load_result_list(method, "2023-24", unit_level_name)
  res_2425 <- load_result_list(method, "2024-25", unit_level_name)
  res_2526 <- load_result_list(method, "2025-26", unit_level_name)
  
  names(res_2324) <- paste0(names(res_2324), "_2324")
  names(res_2425) <- paste0(names(res_2425), "_2425")
  names(res_2526) <- paste0(names(res_2526), "_2526")
  
  res_all <- c(res_2324, res_2425, res_2526)
  
  message("Combining df_all_wide...")
  
  all_wide <- purrr::imap_dfr(
    res_all,
    ~ .x$df_all_wide %>%
      dplyr::mutate(result_id = .y)
  )
  
  message("Check result_id...")
  stopifnot("result_id" %in% names(all_wide))
  
  message("Creating summary figures...")
  
  fig_2324 <- summary_figure(
    all_wide,
    paste0("county_", method),
    unit_level_name = "county",
    unit_label = "County",
    season_select = "2324"
  )
  
  fig_2425 <- summary_figure(
    all_wide,
    paste0("county_", method),
    unit_level_name = "county",
    unit_label = "County",
    season_select = "2425"
  )
  
  fig_2526 <- summary_figure(
    all_wide,
    paste0("county_", method),
    unit_level_name = "county",
    unit_label = "County",
    season_select = "2526"
  )
  
  fig_overall <- summary_figure(
    all_wide,
    paste0(unit_level_name, "_", method),
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    season_select = "overall"
  )
  
  summary_dir <- file.path(figure_dir, "summary")
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_pdf <- file.path(
    summary_dir,
    paste0("summary_metrics_", unit_level_name, "_", method, ".pdf")
  )
  
  message("Saving PDF: ", out_pdf)
  
  pdf(out_pdf, width = 15, height = 10)
  
  for (fig in list(fig_2324, fig_2425, fig_2526, fig_overall)) {
    print(fig$p1)
    print(fig$p2)
  }
  
  dev.off()
  
  message("Saved: ", out_pdf)
}

purrr::walk(
  c("redcap"),
  ~ make_method_summary_pdf(
    method = .x,
    unit_level_name = "county",
    unit_label = "County"
  )
)



#############################################################
