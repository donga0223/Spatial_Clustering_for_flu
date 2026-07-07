library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

method_label <- Sys.getenv("SPATIAL_METHOD", unset = "clustergeoaug")
cluster_dir <- Sys.getenv("CLUSTER_DATA_DIR", unset = "data/cluster_data_season")
results_dir <- Sys.getenv("RESULTS_DIR", unset = "results")
figure_dir <- Sys.getenv("FIGURE_DIR", unset = "figures/summary")

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

rac <- read.csv("data/tx_rac.csv") %>%
  dplyr::transmute(
    county = trimws(County),
    rac = as.character(RAC)
  )

dshs <- read.csv("data/tx_dshs_region.csv") %>%
  dplyr::transmute(
    county = trimws(county),
    dshs = as.character(dshs_region)
  )

hsa <- read.csv("data/tx_hsa.csv") %>%
  dplyr::transmute(
    county = trimws(county),
    hsa = as.character(hsa_nci_id)
  )

geo_meta <- rac %>%
  dplyr::left_join(dshs, by = "county") %>%
  dplyr::left_join(hsa, by = "county")

calc_lambda <- function(obs, mapping, level_col, geo_level, k_value) {
  map <- mapping %>%
    dplyr::select(county, region_id = dplyr::all_of(level_col)) %>%
    dplyr::mutate(region_id = as.character(region_id))
  
  obs %>%
    dplyr::left_join(map, by = "county") %>%
    dplyr::filter(
      !is.na(region_id),
      !is.na(value),
      !is.na(population),
      population > 0
    ) %>%
    dplyr::group_by(season, target_end_date, region_id) %>%
    dplyr::mutate(
      inc_region = sum(population * value, na.rm = TRUE) /
        sum(population, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, target_end_date) %>%
    dplyr::mutate(
      inc_state = sum(population * value, na.rm = TRUE) /
        sum(population, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, target_end_date) %>%
    dplyr::summarise(
      numerator = sum(population * (value - inc_region)^2, na.rm = TRUE),
      denominator = sum(population * (value - inc_state)^2, na.rm = TRUE),
      lambda_week = dplyr::if_else(
        denominator > 0,
        1 - numerator / denominator,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::summarise(
      lambda_K = mean(lambda_week, na.rm = TRUE),
      n_weeks = sum(!is.na(lambda_week)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      geo_level = geo_level,
      K = k_value
    )
}

file_pattern <- paste0(
  "^df_county_",
  method_label,
  "_exclude_[0-9]{4}-[0-9]{2}_[0-9]+\\.csv$"
)

cluster_files <- list.files(
  cluster_dir,
  pattern = file_pattern,
  full.names = TRUE
)

if (length(cluster_files) == 0) {
  stop("No clustering files found for method label: ", method_label)
}

by_season <- purrr::map_dfr(cluster_files, function(f_path) {
  parsed <- stringr::str_match(
    basename(f_path),
    "exclude_([0-9]{4})-([0-9]{2})_([0-9]+)\\.csv$"
  )
  
  test_season <- paste0(parsed[, 2], "/", parsed[, 3])
  k <- as.integer(parsed[, 4])
  
  obs <- read.csv(f_path) %>%
    dplyr::mutate(
      county = trimws(county),
      target_end_date = as.Date(target_end_date),
      month = as.integer(format(target_end_date, "%m")),
      season = as.character(season),
      population = as.numeric(population),
      value = as.numeric(value)
    ) %>%
    dplyr::filter(
      season == test_season,
      month %in% c(10, 11, 12, 1, 2, 3)
    )
  
  mapping <- obs %>%
    dplyr::distinct(county, cluster) %>%
    dplyr::mutate(cluster = paste0("G_", cluster)) %>%
    dplyr::left_join(geo_meta, by = "county")
  
  dplyr::bind_rows(
    calc_lambda(obs, mapping, "cluster", "cluster", k),
    calc_lambda(obs, mapping, "rac", "rac", 22),
    calc_lambda(obs, mapping, "dshs", "dshs", 8),
    calc_lambda(obs, mapping, "hsa", "hsa", 61),
    tibble::tibble(
      season = test_season,
      lambda_K = 0,
      n_weeks = dplyr::n_distinct(obs$target_end_date),
      geo_level = "state",
      K = 1
    ),
    tibble::tibble(
      season = test_season,
      lambda_K = 1,
      n_weeks = dplyr::n_distinct(obs$target_end_date),
      geo_level = "county",
      K = 254
    )
  ) %>%
    dplyr::mutate(
      test_season = test_season,
      method = method_label
    )
})

overall <- by_season %>%
  dplyr::group_by(method, geo_level, K) %>%
  dplyr::summarise(
    lambda_K = mean(lambda_K, na.rm = TRUE),
    n_weeks = sum(n_weeks, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    test_season = "Overall",
    season = "Overall"
  )

spatial_variation <- dplyr::bind_rows(by_season, overall) %>%
  dplyr::select(
    method,
    test_season,
    season,
    geo_level,
    K,
    lambda_K,
    n_weeks
  )

csv_path <- file.path(results_dir, paste0("spatial_variation_", method_label, ".csv"))
write.csv(spatial_variation, csv_path, row.names = FALSE)

plot_df <- spatial_variation %>%
  dplyr::filter(test_season == "Overall") %>%
  dplyr::mutate(
    geo_level = factor(
      geo_level,
      levels = c("state", "dshs", "rac", "cluster", "hsa", "county")
    )
  )

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = K, y = lambda_K, color = geo_level, shape = geo_level)
) +
  ggplot2::geom_line(
    data = plot_df %>% dplyr::filter(geo_level == "cluster"),
    ggplot2::aes(group = geo_level),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_x_continuous(breaks = sort(unique(plot_df$K))) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x = "Number of regions (K)",
    y = "Spatial variation retained (lambda_K)",
    color = "Geo level",
    shape = "Geo level",
    title = "Population-weighted spatial variation, flu season months",
    subtitle = paste0(method_label, " clusters; overall across test seasons")
  )

png_path <- file.path(figure_dir, paste0("spatial_variation_", method_label, ".png"))
pdf_path <- file.path(figure_dir, paste0("spatial_variation_", method_label, ".pdf"))

ggplot2::ggsave(png_path, p, width = 9, height = 6, dpi = 180)
ggplot2::ggsave(pdf_path, p, width = 9, height = 6)

print(
  spatial_variation %>%
    dplyr::filter(test_season == "Overall") %>%
    dplyr::arrange(K, geo_level)
)

message("Saved: ", csv_path)
message("Saved: ", png_path)
message("Saved: ", pdf_path)

