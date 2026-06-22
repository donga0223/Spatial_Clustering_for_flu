library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(ggplot2)

source("code/calculate_spatial_variation_observed.R")

# ==============================================================================
# 1. Data Loading and Mapping Preparation (Existing logic remains identical)
# ==============================================================================
rac_df    <- read.csv("data/tx_rac.csv") %>% rename(county = County)
dshs_meta <- read.csv("data/tx_dshs_region.csv")
hsa_meta  <- read.csv("data/tx_hsa.csv")

dir_season       <- "data/cluster_data_season2"
file_list_season <- list.files(dir_season, pattern = "_all\\.csv$", full.names = TRUE)

all_results <- purrr::map_dfr(file_list_season, function(f_path) {
  f_name <- basename(f_path)
  parsed_info <- stringr::str_match(f_name, "df_county_([a-zA-Z0-9]+)_exclude_([0-9]{4}-[0-9]{2})_([0-9]+)_all\\.csv")
  if (is.na(parsed_info[1])) return(NULL)
  
  curr_method  <- parsed_info[2]
  excluded_sea <- stringr::str_replace(parsed_info[3], "-", "/")
  curr_k_val   <- as.numeric(parsed_info[4])
  
  obs_all  <- read.csv(f_path)
  obs_test <- obs_all %>% dplyr::filter(season == excluded_sea)
  if (nrow(obs_test) == 0) return(NULL)
  
  meta_name <- stringr::str_replace(f_name, "_all\\.csv$", ".csv")
  meta_path <- file.path(dir_season, meta_name)
  if (!file.exists(meta_path)) return(NULL)
  
  cluster_tmp  <- read.csv(meta_path)
  cluster_meta <- cluster_tmp %>%
    dplyr::select(county, cluster) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cluster = paste0("G_", cluster))
  
  full_mapping_table <- rac_df %>%
    dplyr::select(county, rac = RAC) %>%
    dplyr::left_join(dshs_meta %>% dplyr::select(county, dshs = dshs_region), by = "county") %>%
    dplyr::left_join(hsa_meta  %>% dplyr::select(county, hsa_nci_id), by = "county") %>%
    dplyr::left_join(cluster_meta %>% dplyr::select(county, cluster), by = "county")
  
  calc <- function(peak_only) {
    calculate_spatial_variation_observed(
      obs_data = obs_test, mapping_df = full_mapping_table,
      agg_levels = c("rac", "dshs", "hsa", "cluster"),
      by_season = FALSE, compute_overall = TRUE, peak_weeks_only = peak_only,
      peak_months = c(10, 11, 12, 1, 2, 3)
    ) %>% dplyr::mutate(period_type = if (peak_only) "Peak Season Only" else "Full Period")
  }
  
  dplyr::bind_rows(calc(FALSE), calc(TRUE)) %>%
    dplyr::mutate(
      method = curr_method, K = curr_k_val, test_season = excluded_sea,
      test_season_label = paste0("Test Season: ", excluded_sea)
    )
})

# Compile Overall summaries
overall_results <- all_results %>%
  dplyr::group_by(method, K, geo_level, period_type) %>%
  dplyr::summarise(lambda_K = mean(lambda_K, na.rm = TRUE), n_weeks = sum(n_weeks, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(test_season = "Overall", test_season_label = "Overall")

all_results_final <- dplyr::bind_rows(all_results, overall_results)

# ==============================================================================
# 2. 🌟 NEW: Unified Aesthetics for all 6 Spatial Partitioning Methods
# ==============================================================================
# Define clear factors and map exact names to match data levels
all_methods_order <- c("clustergeo", "redcap", "skater", "dshs", "rac", "hsa")
all_methods_labels <- c("clustergeo", "redcap", "skater", "DSHS", "RAC", "HSA")

# Single unified color palette combining clustering algorithms and administrative boundaries
unified_colors <- c(
  clustergeo = "#CC79A7", redcap = "#D55E00", skater = "#0072B2",
  dshs       = "#E69F00", rac    = "#56B4E9", hsa    = "#009E73"
)

# Unified linetype mapping (Lines for models, specific styles for references)
unified_lty <- c(
  clustergeo = "solid", redcap = "solid", skater = "solid",
  dshs       = "solid", rac    = "dashed", hsa    = "dotted"
)

# Split and re-factor datasets based on the unified keys
plot_cluster_data <- all_results_final %>% 
  dplyr::filter(geo_level == "cluster") %>%
  dplyr::mutate(method_factor = factor(method, levels = all_methods_order))

plot_baseline_data <- all_results_final %>%
  dplyr::filter(geo_level %in% c("dshs", "rac", "hsa")) %>%
  dplyr::mutate(method_factor = factor(geo_level, levels = all_methods_order))


# ==============================================================================
# 📊 Plot 1: Spatial Variation - Overall
# ==============================================================================
p1_cluster  <- plot_cluster_data  %>% dplyr::filter(test_season == "Overall")
p1_baseline <- plot_baseline_data %>% dplyr::filter(test_season == "Overall")

ggplot() +
  # Fixed administrative boundaries represented as clean guide lines
  geom_hline(data = p1_baseline,
             aes(yintercept = lambda_K, color = method_factor, linetype = method_factor),
             linewidth = 1.2, alpha = 0.85) +
  # Clustering models represented as points and lines across K size
  geom_line(data = p1_cluster,
            aes(x = K, y = lambda_K, color = method_factor, group = method_factor),
            linewidth = 1) +
  geom_point(data = p1_cluster,
             aes(x = K, y = lambda_K, color = method_factor),
             size = 2.5) +
  facet_wrap(~ period_type) +
  # 🌟 Pure unified scale mapping (No duplicated legends, single integrated layout)
  scale_color_manual(values = unified_colors, breaks = all_methods_order, labels = all_methods_labels) +
  scale_linetype_manual(values = unified_lty, breaks = all_methods_order, labels = all_methods_labels) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold"),
    panel.border    = element_rect(color = "gray85", fill = NA)
  ) +
  scale_x_continuous(breaks = unique(p1_cluster$K)) +
  labs(
    title    = "Spatial Variation: Overall (Test Seasons Only)",
    subtitle = "Lambda calculated on held-out test season for each excluded year",
    x        = "Number of Clusters (K)",
    y        = expression(bar(Lambda)[K]),
    color    = "Method",
    linetype = "Method"
  )

ggsave("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/lambda_overall_test_only.png", width = 12, height = 5, dpi = 150)

# ==============================================================================
# 📊 Plot 2: Spatial Variation - By Test Season
# ==============================================================================
p2_cluster  <- plot_cluster_data  %>% dplyr::filter(test_season != "Overall")
p2_baseline <- plot_baseline_data %>% dplyr::filter(test_season != "Overall")

ggplot() +
  geom_hline(data = p2_baseline,
             aes(yintercept = lambda_K, color = method_factor, linetype = method_factor),
             linewidth = 1.2, alpha = 0.85) +
  geom_line(data = p2_cluster,
            aes(x = K, y = lambda_K, color = method_factor, group = method_factor),
            linewidth = 1) +
  geom_point(data = p2_cluster,
             aes(x = K, y = lambda_K, color = method_factor),
             size = 2) +
  facet_grid(test_season_label ~ period_type) +
  scale_color_manual(values = unified_colors, breaks = all_methods_order, labels = all_methods_labels) +
  scale_linetype_manual(values = unified_lty, breaks = all_methods_order, labels = all_methods_labels) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold", size = 10),
    panel.border    = element_rect(color = "gray85", fill = NA)
  ) +
  scale_x_continuous(breaks = unique(p2_cluster$K)) +
  labs(
    title    = "Spatial Variation: By Test Season",
    subtitle = "Each row shows Lambda computed on that season's held-out test data only",
    x        = "Number of Clusters (K)",
    y        = expression(bar(Lambda)[K]),
    color    = "Method",
    linetype = "Method"
  )

ggsave("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/lambda_by_test_season.png", width = 12, height = 10, dpi = 150)

message("Done. Plots saved successfully.")
