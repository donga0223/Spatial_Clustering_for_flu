library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

methods <- Sys.getenv("SPATIAL_METHODS", unset = "clustergeo,clustergeoaug") %>%
  strsplit(",") %>%
  unlist() %>%
  trimws()

cluster_dir <- Sys.getenv("CLUSTER_DATA_DIR", unset = "data/cluster_data")
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
    dplyr::group_by(season, Date, region_id) %>%
    dplyr::mutate(
      inc_region = sum(population * value, na.rm = TRUE) /
        sum(population, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, Date) %>%
    dplyr::mutate(
      inc_state = sum(population * value, na.rm = TRUE) /
        sum(population, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, Date) %>%
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

read_one_cluster_file <- function(f_path, method_label) {
  parsed <- stringr::str_match(
    basename(f_path),
    paste0("^df_county_", method_label, "_([0-9]+)\\.csv$")
  )
  
  if (is.na(parsed[, 1])) {
    stop("File name does not match expected whole-data pattern: ", basename(f_path))
  }
  
  k <- as.integer(parsed[, 2])
  
  obs <- read.csv(f_path) %>%
    dplyr::mutate(
      county = trimws(county),
      Date = as.Date(Date),
      month = as.integer(format(Date, "%m")),
      season = as.character(season),
      population = as.numeric(population),
      value = as.numeric(value)
    ) %>%
    dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3))
  
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
      lambda_K = 0,
      n_weeks = dplyr::n_distinct(paste(obs$season, obs$Date)),
      geo_level = "state",
      K = 1
    ),
    tibble::tibble(
      lambda_K = 1,
      n_weeks = dplyr::n_distinct(paste(obs$season, obs$Date)),
      geo_level = "county",
      K = 254
    )
  ) %>%
    dplyr::mutate(
      method = method_label,
      period = "All training data"
    )
}

spatial_variation <- purrr::map_dfr(methods, function(method_label) {
  cluster_files <- list.files(
    cluster_dir,
    pattern = paste0("^df_county_", method_label, "_[0-9]+\\.csv$"),
    full.names = TRUE
  )
  
  if (length(cluster_files) == 0) {
    warning("No whole-data clustering files found for method: ", method_label)
    return(tibble::tibble())
  }
  
  purrr::map_dfr(cluster_files, read_one_cluster_file, method_label = method_label)
})

if (nrow(spatial_variation) == 0) {
  stop("No spatial variation results were generated.")
}

spatial_variation <- spatial_variation %>%
  dplyr::group_by(method, period, geo_level, K) %>%
  dplyr::summarise(
    lambda_K = mean(lambda_K, na.rm = TRUE),
    n_weeks = max(n_weeks, na.rm = TRUE),
    .groups = "drop"
  )

csv_path <- file.path(results_dir, "spatial_variation_whole_data.csv")
write.csv(spatial_variation, csv_path, row.names = FALSE)

plot_df <- spatial_variation %>%
  dplyr::mutate(
    geo_level = factor(
      geo_level,
      levels = c("state", "dshs", "rac", "cluster", "hsa", "county")
    )
  )

p <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = K, y = lambda_K, color = geo_level, shape = method)
) +
  ggplot2::geom_line(
    data = plot_df %>% dplyr::filter(geo_level == "cluster"),
    ggplot2::aes(group = method, linetype = method),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_color_manual(
    values = c(
      state = "#4d4d4d",
      dshs = "#e41a1c",
      rac = "#377eb8",
      cluster = "#4daf4a",
      hsa = "#984ea3",
      county = "#ff7f00"
    )
  ) +
  ggplot2::scale_x_continuous(breaks = sort(unique(plot_df$K))) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(
    x = "Number of regions (K)",
    y = "Spatial variation retained (lambda_K)",
    color = "Geo level",
    shape = "Method",
    linetype = "Method",
    title = "Population-weighted spatial variation, flu season months",
    subtitle = "Whole-data clusters from data/cluster_data"
  )

png_path <- file.path(figure_dir, "spatial_variation_whole_data.png")
pdf_path <- file.path(figure_dir, "spatial_variation_whole_data.pdf")

ggplot2::ggsave(png_path, p, width = 10, height = 6.5, dpi = 180)
ggplot2::ggsave(pdf_path, p, width = 10, height = 6.5)

print(
  spatial_variation %>%
    dplyr::arrange(K, geo_level, method)
)

message("Saved: ", csv_path)
message("Saved: ", png_path)
message("Saved: ", pdf_path)
