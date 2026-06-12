load("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/summary_county_clustergeo.RData")
source("code/forecasting_summary_figure_function.R")
summary_county_clustergeo_all <- purrr::map_dfr(
  res_county_clustergeo_list,
  "summary_metrics",
  .id = "result_id"
)



p1 <- plot_summary_metrics_same_level(
  summary_all = summary_county_clustergeo_all,
  method_name = "county clustergeo",
  unit_level_name = "county",
  unit_label = "County"
)

p2 <- plot_summary_metrics_vs_unit(
  summary_all = summary_county_clustergeo_all,
  method_name = "county_clustergeo",
  unit_level_name = "county",
  unit_label = "County"
)

p1
p2

summary_county_clustergeo_spatial_var <- purrr::map_dfr(
  res_county_clustergeo_list,
  "spatial_var_metrics",
  .id = "result_id"
)


p_spatial <- plot_spatial_variation_vs_state(
  spatial_var = summary_county_clustergeo_spatial_var,
  method_name = "clustergeo"
)

p_spatial


if(2==3){
  idx <- which(purrr::map_int(res_county_clustergeo_list, "n_cluster") %in% c(13, 14, 15))
  
  purrr::map(
    idx,
    ~ table(res_county_clustergeo_list[[.x]]$obs$cluster)
  )
  
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      result_id %in% c("k13", "k14"),
      geo_level == "G"
    ) %>%
    ggplot(
      aes(
        x = target_end_date,
        y = spatial_variation_preserved,
        color = result_id
      )
    ) +
    geom_line() +
    facet_wrap(~ horizon)
  
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      result_id %in% c("k13", "k14", "k15"),
      geo_level == "G"
    ) %>%
    mutate(
      season = case_when(
        target_end_date < as.Date("2024-08-01") ~ "2023/24",
        target_end_date < as.Date("2025-08-01") ~ "2024/25",
        TRUE ~ "2025/26"
      )
    ) %>%
    group_by(
      season,
      result_id,
      horizon
    ) %>%
    summarise(
      mean_spatial =
        mean(spatial_variation_preserved),
      .groups = "drop"
    ) %>%
    print(n = 36)
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      geo_level == "G",
      result_id %in% c("k13","k14","k15")
    ) %>%
    mutate(
      season = case_when(
        target_end_date < as.Date("2024-08-01") ~ "2023/24",
        target_end_date < as.Date("2025-08-01") ~ "2024/25",
        TRUE ~ "2025/26"
      ),
      K = readr::parse_number(result_id)
    ) %>%
    ggplot(
      aes(
        x = K,
        y = spatial_variation_preserved,
        color = factor(horizon)
      )
    ) +
    stat_summary(
      fun = mean,
      geom = "line"
    ) +
    stat_summary(
      fun = mean,
      geom = "point"
    ) +
    facet_wrap(~ season)
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      geo_level == "G",
      result_id %in% c("k13","k14","k15")
    ) %>%
    group_by(result_id) %>%
    summarise(
      n_negative =
        sum(spatial_variation_preserved < 0),
      prop_negative =
        mean(spatial_variation_preserved < 0),
      min_value =
        min(spatial_variation_preserved),
      mean_value =
        mean(spatial_variation_preserved)
    )
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      geo_level == "G"
    ) %>%
    group_by(result_id) %>%
    summarise(
      mean_num = mean(numerator),
      mean_den = mean(denominator),
      mean_ratio = mean(numerator/denominator),
      mean_metric = mean(spatial_variation_preserved)
    ) %>%
    ggplot(aes(x = result_id)) +
    geom_line(aes(y = mean_ratio, color = 'red', group = 1)) 
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      geo_level == "G",
      result_id %in% c("k12", "k13","k14", "k15")
    ) %>%
    ggplot(
      aes(
        x = spatial_variation_preserved,
        fill = result_id
      )
    ) +
    geom_density(alpha = .3)
  
  
  idx13 <- which(
    purrr::map_int(
      res_county_clustergeo_list,
      "n_cluster"
    ) == 13
  )
  
  idx14 <- which(
    purrr::map_int(
      res_county_clustergeo_list,
      "n_cluster"
    ) == 14
  )
  
  obs13 <- res_county_clustergeo_list[[idx13]]$obs
  obs14 <- res_county_clustergeo_list[[idx14]]$obs
  
  c13 <- obs13 %>%
    distinct(county, cluster)
  
  c14 <- obs14 %>%
    distinct(county, cluster)
  
  table(
    c13$cluster,
    c14$cluster
  )
  
  summary_county_clustergeo_spatial_var %>%
    filter(
      geo_level == "G",
      result_id %in% c("k13","k14")
    ) %>%
    group_by(result_id) %>%
    summarise(
      mean = mean(spatial_variation_preserved),
      median = median(spatial_variation_preserved),
      q10 = quantile(spatial_variation_preserved, .10),
      q90 = quantile(spatial_variation_preserved, .90)
    )


  }

