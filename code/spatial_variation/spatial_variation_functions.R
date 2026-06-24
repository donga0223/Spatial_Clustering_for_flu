library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

# ==========================================================
# Helper: Build geo metadata table
# ==========================================================
build_geo_meta <- function(rac_meta, dshs_meta, hsa_meta) {
  
  geo_meta <- rac_meta %>%
    dplyr::rename(county = County) %>%
    dplyr::select(county, rac = RAC) %>%
    dplyr::left_join(
      dshs_meta %>% dplyr::select(county, dshs = dshs_region),
      by = "county"
    ) %>%
    dplyr::left_join(
      hsa_meta %>% dplyr::select(county, hsa_nci_id),
      by = "county"
    ) %>%
    dplyr::mutate(county = trimws(county))
  
  return(geo_meta)
}


# ==========================================================
# Helper: Parse clustering filename
# ==========================================================
parse_cluster_filename <- function(file_name) {
  
  parsed <- stringr::str_match(
    file_name,
    "df_county_([a-zA-Z0-9]+)_exclude_([0-9]{4}-[0-9]{2})_([0-9]+)_all\\.csv"
  )
  
  if (is.na(parsed[1])) return(NULL)
  
  list(
    method = parsed[2],
    excluded_season = stringr::str_replace(parsed[3], "-", "/"),
    K = as.numeric(parsed[4])
  )
}


# ==========================================================
# Helper: Prepare mapping table
# ==========================================================
prepare_mapping_table <- function(cluster_meta, geo_meta) {
  
  mapping_df <- geo_meta %>%
    dplyr::left_join(
      cluster_meta %>% dplyr::select(county, cluster),
      by = "county"
    )
  
  return(mapping_df)
}


# ==========================================================
# Helper: Load spatial variation case
# ==========================================================

load_spatial_variation_case <- function(f_path,
                                        dir_season,
                                        geo_meta) {
  
  f_name <- basename(f_path)
  parsed <- parse_cluster_filename(f_name)
  
  if (is.null(parsed)) {
    stop("File name does not match expected pattern: ", f_name)
  }
  
  obs_all <- read.csv(f_path)
  
  obs_test <- obs_all %>%
    dplyr::filter(season == parsed$excluded_season)
  
  if (nrow(obs_test) == 0) {
    stop("No rows found for excluded season: ", parsed$excluded_season)
  }
  
  meta_name <- stringr::str_replace(f_name, "_all\\.csv$", ".csv")
  meta_path <- file.path(dir_season, meta_name)
  
  if (!file.exists(meta_path)) {
    stop("Matching cluster metadata file does not exist: ", meta_path)
  }
  
  cluster_tmp <- read.csv(meta_path)
  
  cluster_meta <- cluster_tmp %>%
    dplyr::select(county, cluster) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      county = trimws(county),
      cluster = paste0("G_", cluster)
    )
  
  mapping_table <- prepare_mapping_table(
    cluster_meta = cluster_meta,
    geo_meta = geo_meta
  )
  
  return(list(
    obs_all = obs_all,
    obs_test = obs_test,
    mapping_table = mapping_table,
    parsed = parsed,
    file_name = f_name
  ))
}

# ==========================================================
# Internal helper: calculate lambda for one selected period
# ==========================================================
calculate_spatial_variation_one_period <- function(obs_data,
                                                   mapping_df,
                                                   agg_levels = c("cluster", "rac", "dshs", "hsa"),
                                                   weight_col = NULL) {
  
  df_county <- obs_data %>% 
    dplyr::filter(geo_level == "county") %>% 
    dplyr::select(
      county = location,
      season,
      target_end_date,
      inc_county = inc,
      dplyr::any_of(weight_col)
    ) %>%
    dplyr::mutate(
      county = trimws(county),
      target_end_date = as.Date(target_end_date)
    )
  
  if (is.null(weight_col)) {
    
    df_county <- df_county %>%
      dplyr::mutate(weight = 1)
    
    weight_type <- "unweighted"
    
  } else if (weight_col %in% names(df_county)) {
    
    df_county <- df_county %>%
      dplyr::mutate(weight = as.numeric(.data[[weight_col]]))
    
    weight_type <- weight_col
    
  } else if (weight_col %in% names(mapping_df)) {
    
    df_county <- df_county %>%
      dplyr::left_join(
        mapping_df %>%
          dplyr::select(county, weight = dplyr::all_of(weight_col)),
        by = "county"
      ) %>%
      dplyr::mutate(weight = as.numeric(weight))
    
    weight_type <- weight_col
    
  } else {
    stop("weight_col does not exist in obs_data or mapping_df: ", weight_col)
  }
  
  df_county <- df_county %>%
    dplyr::mutate(
      weight = dplyr::if_else(is.na(weight) | weight < 0, 0, weight)
    )
  
  valid_levels <- agg_levels[agg_levels %in% c("cluster", "rac", "dshs", "hsa")]
  
  if (length(valid_levels) == 0) {
    stop("No valid aggregation levels were provided.")
  }
  
  purrr::map_dfr(valid_levels, function(lev) {
    
    target_col <- lev
    
    if (lev == "hsa" &&
        !("hsa" %in% names(mapping_df)) &&
        ("hsa_nci_id" %in% names(mapping_df))) {
      target_col <- "hsa_nci_id"
    }
    
    if (!(target_col %in% names(mapping_df))) {
      warning("Skipping level because mapping column does not exist: ", lev)
      return(NULL)
    }
    
    df_map_subset <- mapping_df %>% 
      dplyr::select(county, region_id = dplyr::all_of(target_col)) %>% 
      dplyr::mutate(
        county = trimws(county),
        region_id = as.character(region_id)
      )
    
    df_weekly_comp <- df_county %>% 
      dplyr::left_join(df_map_subset, by = "county") %>%
      dplyr::filter(
        !is.na(inc_county),
        !is.na(region_id),
        !is.na(weight),
        weight > 0
      ) %>%
      dplyr::group_by(season, target_end_date, region_id) %>%
      dplyr::mutate(
        inc_region = sum(weight * inc_county, na.rm = TRUE) /
          sum(weight[!is.na(inc_county)], na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(season, target_end_date) %>%
      dplyr::mutate(
        inc_state = sum(weight * inc_county, na.rm = TRUE) /
          sum(weight[!is.na(inc_county)], na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        !is.na(inc_region),
        !is.na(inc_state)
      )
    
    df_weekly_ratio <- df_weekly_comp %>% 
      dplyr::group_by(season, target_end_date) %>% 
      dplyr::summarise(
        numerator = sum(weight * (inc_county - inc_region)^2, na.rm = TRUE),
        denominator = sum(weight * (inc_county - inc_state)^2, na.rm = TRUE),
        total_weight = sum(weight, na.rm = TRUE),
        weekly_val = dplyr::if_else(
          denominator > 0 & total_weight > 0,
          1 - (numerator / denominator),
          NA_real_
        ),
        .groups = "drop"
      ) %>% 
      dplyr::filter(!is.na(weekly_val))
    
    df_weekly_ratio %>% 
      dplyr::group_by(season) %>% 
      dplyr::summarise(
        lambda_K = mean(weekly_val, na.rm = TRUE),
        n_weeks = dplyr::n(),
        .groups = "drop"
      ) %>% 
      dplyr::mutate(
        geo_level = lev,
        weight_type = weight_type,
        type = "by_season"
      ) %>% 
      dplyr::select(
        geo_level,
        type,
        season,
        lambda_K,
        n_weeks,
        weight_type
      )
  })
}
# ==========================================================
# Main function: calculate lambda for Full + Flu Season Months
# ==========================================================
calculate_spatial_variation <- function(obs_data,
                                        mapping_df,
                                        agg_levels = c("cluster", "rac", "dshs", "hsa"),
                                        season_months = c(10, 11, 12, 1, 2, 3),
                                        weight_col = NULL) {
  
  obs_data <- obs_data %>% 
    dplyr::mutate(
      target_end_date = as.Date(target_end_date),
      month = as.numeric(format(target_end_date, "%m"))
    )
  
  period_settings <- tibble::tibble(
    period_type = c("Full Period", "Flu Season Months"),
    use_month_filter = c(FALSE, TRUE)
  )
  
  purrr::map_dfr(seq_len(nrow(period_settings)), function(ii) {
    
    curr_period <- period_settings$period_type[ii]
    use_month_filter <- period_settings$use_month_filter[ii]
    
    obs_period <- obs_data
    
    if (use_month_filter) {
      obs_period <- obs_period %>%
        dplyr::filter(month %in% season_months)
    }
    
    if (nrow(obs_period) == 0) {
      warning("No rows available for period: ", curr_period)
      return(NULL)
    }
    
    calculate_spatial_variation_one_period(
      obs_data = obs_period,
      mapping_df = mapping_df,
      agg_levels = agg_levels,
      weight_col = weight_col
    ) %>%
      dplyr::mutate(period_type = curr_period)
  }) %>%
    dplyr::select(
      geo_level,
      period_type,
      type,
      season,
      lambda_K,
      n_weeks,
      weight_type
    )
}

# ==========================================================
# Main: Compile spatial variation
# ==========================================================
compile_spatial_variation_results <- function(dir_season,
                                              geo_meta,
                                              weight_col = NULL) {
  
  file_list <- list.files(
    dir_season,
    pattern = "_all\\.csv$",
    full.names = TRUE
  )
  
  message("Directory: ", dir_season)
  message("Working directory: ", getwd())
  message("Number of files found: ", length(file_list))
  
  if (length(file_list) == 0) {
    stop(
      "No *_all.csv files found. Check dir_season or current working directory.\n",
      "dir_season = ", dir_season, "\n",
      "getwd() = ", getwd()
    )
  }
  
  n_files <- length(file_list)
  
  message("======================================")
  message("Starting spatial variation compilation")
  message("Directory: ", dir_season)
  message("Total files: ", n_files)
  message("Weight type: ", ifelse(is.null(weight_col), "unweighted", weight_col))
  message("======================================")
  
  by_season_results <- purrr::imap_dfr(file_list, function(f_path, idx) {
    
    file_name <- basename(f_path)
    
    message("\n[", idx, "/", n_files, "] Processing: ", file_name)
    
    start_time <- Sys.time()
    
    case <- load_spatial_variation_case(
      f_path = f_path,
      dir_season = dir_season,
      geo_meta = geo_meta
    )
    
    message(
      "  Method: ", case$parsed$method,
      " | K=", case$parsed$K,
      " | Test season=", case$parsed$excluded_season
    )
    
    res <- calculate_spatial_variation(
      obs_data = case$obs_test,
      mapping_df = case$mapping_table,
      agg_levels = c("rac", "dshs", "hsa", "cluster"),
      season_months = c(10, 11, 12, 1, 2, 3),
      weight_col = weight_col
    )
    
    message("  Result rows before mutate: ", nrow(res))
    
    if (nrow(res) == 0) {
      warning("  No result generated for this file: ", file_name)
      return(NULL)
    }
    
    res <- res %>%
      dplyr::mutate(
        method = case$parsed$method,
        K = case$parsed$K,
        test_season = case$parsed$excluded_season,
        test_season_label = paste0("Test Season: ", case$parsed$excluded_season)
      )
    
    elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
    message("  Done (", elapsed, " sec)")
    
    res
  })
  
  message("\nFinished processing all files.")
  message("by_season_results rows: ", nrow(by_season_results))
  message("by_season_results cols: ", paste(names(by_season_results), collapse = ", "))
  
  if (nrow(by_season_results) == 0 || !"method" %in% names(by_season_results)) {
    stop(
      "No by-season results were generated. ",
      "Most likely calculate_spatial_variation() returned 0 rows for every file. ",
      "Check county matching, mapping columns, obs_test geo_level == 'county', or weight_col."
    )
  }
  
  overall_results <- by_season_results %>%
    dplyr::group_by(
      method,
      K,
      geo_level,
      period_type,
      weight_type
    ) %>%
    dplyr::summarise(
      lambda_K = mean(lambda_K, na.rm = TRUE),
      n_weeks = sum(n_weeks, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      type = "overall_across_test_seasons",
      season = "Overall",
      test_season = "Overall",
      test_season_label = "Overall"
    )
  
  final_res <- dplyr::bind_rows(
    by_season_results,
    overall_results
  ) %>%
    dplyr::select(
      method,
      K,
      geo_level,
      period_type,
      type,
      season,
      test_season,
      test_season_label,
      lambda_K,
      n_weeks,
      weight_type
    )
  
  message("Done. Final rows: ", nrow(final_res))
  
  return(final_res)
}


