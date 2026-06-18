library(dplyr)
library(purrr)
library(tidyr)

#' Advanced Spatial Variation Metric Calculator (Lambda K)
#'
#' @param obs_data Cleaned long format data (containing geo_level: county, state, rac, dshs, hsa, etc.)
#' @param mapping_df A crosswalk table linking counties to region IDs
#' @param agg_levels Character vector of aggregation levels to process
#' @param by_season Logical. If TRUE, computes metrics broken down by season.
#' @param compute_overall Logical. If TRUE, computes metrics for the entire timeframe combined.
#' @param peak_weeks_only Logical. If TRUE, filters dates to keep only the peak season weeks.
#' @param peak_months Numeric vector. Months considered as 'peak' (Default: Dec, Jan, Feb -> c(12, 1, 2))
#'
calculate_spatial_variation_observed <- function(obs_data, 
                                                 mapping_df, 
                                                 agg_levels = c("cluster", "rac", "dshs", "hsa"),
                                                 by_season = TRUE,
                                                 compute_overall = TRUE,
                                                 peak_weeks_only = FALSE,
                                                 peak_months = c(12, 1, 2)) {
  
  # 1. Standardize Date format and extract numerical month for peak season filtering
  obs_data <- obs_data %>% 
    dplyr::mutate(
      target_end_date = as.Date(target_end_date),
      month = as.numeric(format(target_end_date, "%m"))
    )
  
  # Optional: Filter for peak season weeks only (e.g., specific winter months)
  if (peak_weeks_only) {
    obs_data <- obs_data %>% 
      dplyr::filter(month %in% peak_months)
    message("ℹ️ Filtering data for peak season months: ", paste(peak_months, collapse = ", "))
  }
  
  # 2. Extract unit level (county) and baseline level (state) curves
  df_county <- obs_data %>% 
    dplyr::filter(geo_level == "county") %>% 
    dplyr::select(county = location, season, target_end_date, inc_county = inc)
  
  df_state <- obs_data %>% 
    dplyr::filter(geo_level == "state") %>% 
    dplyr::select(season, target_end_date, inc_state = inc) %>% 
    dplyr::distinct()
  
  # 3. Dynamic level verification to ensure requested levels exist in the dataset
  existing_levels <- agg_levels[agg_levels %in% unique(obs_data$geo_level)]
  if (length(existing_levels) == 0) {
    stop("❌ None of the specified agg_levels match the 'geo_level' values in obs_data.")
  }
  
  # 4. Loop through each valid spatial aggregation level to calculate Lambda_K
  purrr::map_dfr(existing_levels, function(lev) {
    
    # Extract specific region level incidence curves
    df_region <- obs_data %>% 
      dplyr::filter(geo_level == lev) %>% 
      dplyr::select(region_id = location, target_end_date, inc_region = inc)
    
    # 🌟 FIX: Bridge name mismatches between geo_level string ("hsa") and mapping column ("hsa_nci_id")
    target_col <- lev
    if (lev == "hsa" && !("hsa" %in% names(mapping_df)) && ("hsa_nci_id" %in% names(mapping_df))) {
      target_col <- "hsa_nci_id"
    }
    
    # Isolate relevant mapping column dynamically and match with county definitions
    df_map_subset <- mapping_df %>% 
      dplyr::select(county, region_id = dplyr::all_of(target_col)) %>% 
      dplyr::mutate(county = trimws(county), region_id = as.character(region_id))
    
    # Construct unified analytical dataset matching county i, cluster c(i), and state by date
    df_weekly_comp <- df_county %>% 
      dplyr::left_join(df_map_subset, by = "county") %>% 
      dplyr::left_join(df_state, by = c("season", "target_end_date")) %>% 
      dplyr::left_join(df_region, by = c("target_end_date", "region_id")) %>% 
      dplyr::filter(!is.na(inc_county), !is.na(inc_region), !is.na(inc_state))
    
    # Calculate Sum_i elements grouped by Date (Week 'w')
    df_weekly_ratio <- df_weekly_comp %>% 
      dplyr::group_by(season, target_end_date) %>% 
      dplyr::summarise(
        numerator   = sum((inc_county - inc_region)^2, na.rm = TRUE),
        denominator = sum((inc_county - inc_state)^2, na.rm = TRUE),
        weekly_val  = dplyr::if_else(denominator > 0, 1 - (numerator / denominator), NA_real_),
        .groups     = "drop"
      ) %>% 
      dplyr::filter(!is.na(weekly_val))
    
    # Container list to hold requested aggregation structures
    results_list <- list()
    
    # Structure A: Calculate by Season (Arithmetic mean per season group)
    if (by_season) {
      results_list$season_breakdown <- df_weekly_ratio %>% 
        dplyr::group_by(season) %>% 
        dplyr::summarise(
          lambda_K = mean(weekly_val, na.rm = TRUE),
          n_weeks  = dplyr::n(),
          .groups  = "drop"
        ) %>% 
        dplyr::mutate(type = "by_season")
    }
    
    # Structure B: Calculate Overall (Arithmetic mean over the entire timeframe combined)
    if (compute_overall) {
      results_list$overall_summary <- df_weekly_ratio %>% 
        dplyr::summarise(
          lambda_K = mean(weekly_val, na.rm = TRUE),
          n_weeks  = dplyr::n()
        ) %>% 
        dplyr::mutate(season = "Overall", type = "overall")
    }
    
    # Combine individual structures into a single compiled dataframe for this level
    dplyr::bind_rows(results_list) %>% 
      dplyr::mutate(
        geo_level = lev,
        analysis_scope = dplyr::if_else(peak_weeks_only, "Peak Season Only", "Full Period Specified")
      ) %>% 
      dplyr::select(geo_level, type, season, lambda_K, n_weeks, analysis_scope)
  })
}

# ==============================================================================
# 1. Data Loading and Crosswalk Mapping Consolidation
# ==============================================================================
obs_all   <- read.csv("data/cluster_data/df_county_clustergeo_5_all.csv")
rac_df    <- read.csv("data/tx_rac.csv") %>% rename(county = County)
dshs_meta <- read.csv("data/tx_dshs_region.csv") 
hsa_meta  <- read.csv("data/tx_hsa.csv") 

# Build the global master crosswalk table to map counties to all target geo-levels
full_mapping_table <- rac_df %>% 
  dplyr::select(county, rac = RAC) %>% 
  dplyr::left_join(dshs_meta %>% dplyr::select(county, dshs = dshs_region), by = "county") %>% 
  dplyr::left_join(hsa_meta  %>% dplyr::select(county, hsa_nci_id), by = "county")


# ==============================================================================
# 2. Execution Scenario Testing 
# ==============================================================================

# Scenario 1: Calculate full period metrics (Using "hsa" to match geo_level values)
metrics_normal <- calculate_spatial_variation_observed(
  obs_data        = obs_all, 
  mapping_df      = full_mapping_table, 
  agg_levels      = c("rac", "dshs", "hsa"), # 🌟 Changed "hsa_nci_id" to "hsa"
  by_season       = TRUE,
  compute_overall = TRUE,
  peak_weeks_only = FALSE
)

print("--- [Scenario 1: Normal Full Period] ---")
print(metrics_normal)


# Scenario 2: Calculate peak season metrics
metrics_peak_only <- calculate_spatial_variation_observed(
  obs_data        = obs_all, 
  mapping_df      = full_mapping_table, 
  agg_levels      = c("rac", "dshs", "hsa"), # 🌟 Changed "hsa_nci_id" to "hsa"
  by_season       = TRUE,
  compute_overall = TRUE,
  peak_weeks_only = TRUE,       
  peak_months     = c(11, 12, 1, 2, 3) 
)

print("--- [Scenario 2: Peak Season Only] ---")
print(metrics_peak_only)




