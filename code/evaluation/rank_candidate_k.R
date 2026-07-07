library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

load_forecast_summary_parts <- function(results_dir,
                                        method_name,
                                        seasons = c("2023-24", "2024-25", "2025-26")) {
  purrr::map_dfr(seasons, function(sea) {
    part_dir <- file.path(results_dir, paste0("summary_parts_", method_name, "_", sea))
    files <- list.files(part_dir, pattern = "^res_k\\d+\\.RData$", full.names = TRUE)
    
    if (length(files) == 0) {
      warning("No summary part files found in: ", part_dir)
      return(NULL)
    }
    
    purrr::map_dfr(files, function(f) {
      env <- new.env(parent = emptyenv())
      load(f, envir = env)
      
      if (!exists("res_k", envir = env)) {
        warning("Skipping file without res_k object: ", f)
        return(NULL)
      }
      
      res <- get("res_k", envir = env)
      k <- as.integer(stringr::str_extract(basename(f), "(?<=res_k)\\d+"))
      
      res$summary_metrics %>%
        dplyr::mutate(
          method_name = method_name,
          season = sea,
          n_cluster = k
        )
    })
  })
}

summarise_cluster_wis <- function(summary_metrics,
                                  wis_col = "WIS_G_vs_county",
                                  horizons = NULL) {
  if (!wis_col %in% names(summary_metrics)) {
    stop("Column not found in summary_metrics: ", wis_col)
  }
  
  out <- summary_metrics
  
  if (!is.null(horizons)) {
    out <- out %>% dplyr::filter(horizon %in% horizons)
  }
  
  out %>%
    dplyr::group_by(method_name, n_cluster) %>%
    dplyr::summarise(
      mean_wis = mean(.data[[wis_col]], na.rm = TRUE),
      median_wis = median(.data[[wis_col]], na.rm = TRUE),
      n_horizon_season = dplyr::n(),
      .groups = "drop"
    )
}

summarise_spatial_variation <- function(spatial_variation,
                                        method_name,
                                        period_type = "Flu Season Months",
                                        weight_type = "population") {
  spatial_variation %>%
    dplyr::filter(
      method == method_name,
      geo_level == "cluster",
      type == "overall_across_test_seasons",
      season == "Overall",
      period_type == !!period_type,
      weight_type == !!weight_type
    ) %>%
    dplyr::transmute(
      method_name = method,
      n_cluster = K,
      lambda_K
    )
}

rank_candidate_k <- function(wis_summary,
                             spatial_summary,
                             wis_weight = 0.7,
                             spatial_weight = 0.3,
                             wis_tolerance = 0.10,
                             top_n = 10) {
  if (wis_weight < 0 || spatial_weight < 0 || (wis_weight + spatial_weight) == 0) {
    stop("wis_weight and spatial_weight must be non-negative and not both zero.")
  }
  
  weights_sum <- wis_weight + spatial_weight
  wis_weight <- wis_weight / weights_sum
  spatial_weight <- spatial_weight / weights_sum
  
  ranked <- wis_summary %>%
    dplyr::left_join(spatial_summary, by = c("method_name", "n_cluster")) %>%
    dplyr::filter(!is.na(mean_wis), !is.na(lambda_K)) %>%
    dplyr::group_by(method_name) %>%
    dplyr::mutate(
      wis_norm = ifelse(
        max(mean_wis) > min(mean_wis),
        (mean_wis - min(mean_wis)) / (max(mean_wis) - min(mean_wis)),
        0
      ),
      spatial_norm = ifelse(
        max(lambda_K) > min(lambda_K),
        (lambda_K - min(lambda_K)) / (max(lambda_K) - min(lambda_K)),
        1
      ),
      composite_score = wis_weight * wis_norm + spatial_weight * (1 - spatial_norm),
      best_wis = min(mean_wis, na.rm = TRUE),
      near_best_wis = mean_wis <= best_wis * (1 + wis_tolerance),
      wis_rank = dplyr::min_rank(mean_wis),
      spatial_rank = dplyr::min_rank(dplyr::desc(lambda_K)),
      composite_rank = dplyr::min_rank(composite_score)
    ) %>%
    dplyr::ungroup()
  
  pareto <- ranked %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pareto_candidate = !any(
        ranked$mean_wis <= mean_wis &
          ranked$lambda_K >= lambda_K &
          (ranked$mean_wis < mean_wis | ranked$lambda_K > lambda_K)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-row_id)
  
  pareto %>%
    dplyr::arrange(composite_rank, wis_rank, spatial_rank) %>%
    dplyr::mutate(shortlist_candidate = dplyr::row_number() <= top_n | near_best_wis | pareto_candidate)
}

write_candidate_k_report <- function(results_dir,
                                     spatial_variation_path,
                                     method_name = "county_clustergeo",
                                     output_csv = NULL,
                                     seasons = c("2023-24", "2024-25", "2025-26"),
                                     horizons = NULL,
                                     wis_col = "WIS_G_vs_county",
                                     period_type = "Flu Season Months",
                                     weight_type = "population",
                                     wis_weight = 0.7,
                                     spatial_weight = 0.3,
                                     wis_tolerance = 0.10,
                                     top_n = 10) {
  summary_metrics <- load_forecast_summary_parts(
    results_dir = results_dir,
    method_name = method_name,
    seasons = seasons
  )
  
  spatial_variation <- readRDS(spatial_variation_path)
  
  ranked <- rank_candidate_k(
    wis_summary = summarise_cluster_wis(
      summary_metrics = summary_metrics,
      wis_col = wis_col,
      horizons = horizons
    ),
    spatial_summary = summarise_spatial_variation(
      spatial_variation = spatial_variation,
      method_name = stringr::str_remove(method_name, "^county_"),
      period_type = period_type,
      weight_type = weight_type
    ),
    wis_weight = wis_weight,
    spatial_weight = spatial_weight,
    wis_tolerance = wis_tolerance,
    top_n = top_n
  )
  
  if (is.null(output_csv)) {
    output_csv <- file.path(results_dir, paste0("candidate_k_", method_name, ".csv"))
  }
  
  readr::write_csv(ranked, output_csv)
  ranked
}

