library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
source("code/calculate_spatial_variation_observed.R")

dir_season <- "data/cluster_data_season2"
file_list_season <- list.files(dir_season, pattern = "_all\\.csv$", full.names = TRUE)

all_results <- purrr::map_dfr(file_list_season, function(f_path) {
  
  f_name <- basename(f_path)
  
  # Parse filename (e.g., df_county_skater_exclude_2023-24_7_all.csv)
  parsed_info <- stringr::str_match(f_name, "df_county_([a-zA-Z0-9]+)_exclude_([0-9]{4}-[0-9]{2})_([0-9]+)_all\\.csv")
  if (is.na(parsed_info[1])) return(NULL) 
  
  curr_method  <- parsed_info[2]
  # Convert file hyphen format (2023-24) to standard season display format (2023/24)
  excluded_sea <- stringr::str_replace(parsed_info[3], "-", "/") 
  curr_k_val   <- as.numeric(parsed_info[4])
  
  # Load the entire long-format observation database (contains all seasons)
  obs_all <- read.csv(f_path)
  
  # Load the matching metadata file for county-cluster mapping boundaries
  meta_name <- stringr::str_replace(f_name, "_all\\.csv$", ".csv")
  meta_path <- file.path(dir_season, meta_name)
  if (!file.exists(meta_path)) return(NULL)
  
  cluster_tmp <- read.csv(meta_path)
  cluster_meta <- cluster_tmp %>%
    dplyr::select(county, cluster) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cluster = paste0("G_", cluster))
  
  # Build master crosswalk mapping table
  full_mapping_table <- rac_df %>% 
    dplyr::select(county, rac = RAC) %>% 
    dplyr::left_join(dshs_meta %>% dplyr::select(county, dshs = dshs_region), by = "county") %>% 
    dplyr::left_join(hsa_meta  %>% dplyr::select(county, hsa_nci_id), by = "county") %>%
    dplyr::left_join(cluster_meta %>% dplyr::select(county, cluster), by = "county") 
  
  # Calculate metrics for ALL seasons inside this file
  m_normal <- calculate_spatial_variation_observed(
    obs_data = obs_all, mapping_df = full_mapping_table, 
    agg_levels = c("rac", "dshs", "hsa", "cluster"), peak_weeks_only = FALSE
  ) %>% dplyr::mutate(period_type = "Full Period")
  
  m_peak <- calculate_spatial_variation_observed(
    obs_data = obs_all, mapping_df = full_mapping_table, 
    agg_levels = c("rac", "dshs", "hsa", "cluster"), peak_weeks_only = TRUE,
    peak_months = c(10, 11, 12, 1, 2, 3)
  ) %>% dplyr::mutate(period_type = "Peak Season Only")
  
  # Combine results and label which season was omitted from this model
  dplyr::bind_rows(m_normal, m_peak) %>%
    dplyr::mutate(
      method = curr_method,
      K = curr_k_val,
      excluded_season = paste0("Excluded: ", excluded_sea) # 🌟 Keep this as a grouping/facet key
    )
  # 🌟 Note: Filter removed. We now retain all calculated seasons (2021/22 ~ 2025/26).
})

# 1. Cluster data (Tracks K on X-axis)
plot_cluster_data <- all_results %>% 
  dplyr::filter(geo_level == "cluster")

# 2. Fixed administrative baselines (RAC, DSHS, HSA)
plot_baseline_data <- all_results %>% 
  dplyr::filter(geo_level != "cluster") %>% 
  dplyr::group_by(geo_level, season, period_type, excluded_season) %>% 
  dplyr::summarise(lambda_K = mean(lambda_K), .groups = "drop")


library(ggplot2)

ggplot() +
  # 1. Plot dynamic lines where each color represents a dataset evaluation year (season)
  geom_line(data = plot_cluster_data, 
            aes(x = K, y = lambda_K, color = season, group = interaction(method, season)), 
            linewidth = 1) +
  geom_point(data = plot_cluster_data, 
             aes(x = K, y = lambda_K, color = season), 
             size = 2) +
  
  # 2. Overlay administrative baseline constraints
  geom_hline(data = plot_baseline_data, 
             aes(yintercept = lambda_K, linetype = geo_level, color = season), 
             alpha = 0.6) +
  
  # 3. Facet Grid Layout (Rows: Model Omission Target / Columns: Period Scope)
  facet_grid(excluded_season ~ period_type) +
  
  # 4. Final plot cosmetics
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_rect(color = "gray85", fill = NA),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  scale_x_continuous(breaks = unique(plot_cluster_data$K)) +
  labs(
    title = "Robustness of Out-of-Season Clusters Across All Observation Years",
    subtitle = "Rows indicate the season omitted during clustering; lines track performance across all tracking seasons.",
    x = "Number of Clusters (K)",
    y = expression(bar(Lambda)[K]),
    color = "Evaluation Season",
    linetype = "Baseline References"
  )


library(ggplot2)

# 예시: 2023/24를 제외하고 클러스터링한 모델 구조만 필터링해서 3개 방법론 비교하기
plot_cluster_subset <- plot_cluster_data %>% 
  dplyr::filter(excluded_season == "Excluded: 2023/24")

plot_baseline_subset <- plot_baseline_data %>% 
  dplyr::filter(excluded_season == "Excluded: 2023/24")

ggplot() +
  # 1. 3가지 방법론을 서로 다른 색상으로 플로팅
  geom_line(data = plot_cluster_subset, 
            aes(x = K, y = lambda_K, color = method, group = method), 
            linewidth = 1) +
  geom_point(data = plot_cluster_subset, 
             aes(x = K, y = lambda_K, color = method), 
             size = 2.5) +
  
  # 2. 행정구역 기준선 추가
  geom_hline(data = plot_baseline_subset, 
             aes(yintercept = lambda_K, linetype = geo_level), 
             color = "gray40", alpha = 0.6) +
  
  # 3. 세로축을 평가 대상 시즌(Season)으로 분할
  facet_grid(season ~ period_type) +
  
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = unique(plot_cluster_subset$K)) +
  labs(
    title = "Method Comparison: Clusters Omit 2023/24 Data",
    subtitle = "Comparing skater, redcap, and kmeans performance across all evaluation seasons",
    x = "Number of Clusters (K)",
    y = expression(bar(Lambda)[K]),
    color = "Clustering Method"
  )



# overall only
plot_cluster_subset <- plot_cluster_data %>% 
  dplyr::filter(season == "Overall")

plot_baseline_subset <- plot_baseline_data %>% 
  dplyr::filter(season == "Overall")

ggplot() +
  # 1. 3가지 방법론을 서로 다른 색상으로 플로팅
  geom_line(data = plot_cluster_subset, 
            aes(x = K, y = lambda_K, color = method, group = method), 
            linewidth = 1) +
  geom_point(data = plot_cluster_subset, 
             aes(x = K, y = lambda_K, color = method), 
             size = 2.5) +
  
  # 2. 행정구역 기준선 추가
  geom_hline(data = plot_baseline_subset, 
             aes(yintercept = lambda_K, linetype = geo_level), 
             color = "gray40", alpha = 0.6) +
  
  # 3. 세로축을 평가 대상 시즌(Season)으로 분할
  facet_grid(excluded_season ~ period_type) +
  
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = unique(plot_cluster_subset$K)) +
  labs(
    title = "Method Comparison: Clusters Omit 2023/24 Data",
    subtitle = "Comparing skater, redcap, and kmeans performance across all evaluation seasons",
    x = "Number of Clusters (K)",
    y = expression(bar(Lambda)[K]),
    color = "Clustering Method"
  )

