sv <- readRDS("/work2/09967/dongahkim0223/frontera/Spatial_clustering/results/spatial_variation_results.rds")
sv %>% filter(type == "overall_across_test_seasons")

sv2 <- sv %>%
  mutate(K = if_else(geo_level == "dshs", 8, K),
         K = if_else(geo_level == "rac", 22, K),
         K = if_else(geo_level == "hsa", 61, K))

sv2 %>% 
  filter(season == "Overall",
         period_type == "Flu Season Months",
         weight_type == "population") %>%
  
  ggplot(aes(x = K, y = lambda_K, group = geo_level, color = geo_level)) +
  geom_point() +
  facet_grid(~weight_type)




## run run_forecasting_summary_figures.R 7-53 lines
all_wide <- purrr::imap_dfr(
  res_all,
  ~ .x$df_all_wide %>%
    dplyr::mutate(result_id = .y)
)

all_wis2 <- all_wide %>% group_by(n_cluster, horizon) %>%
  summarise(MWIS_G = mean(WIS_G), 
            MWIS_county = mean(WIS_county, na.rm = TRUE), 
            MWIS_dshs_region = mean(WIS_dshs_region), 
            MWIS_hsa = mean(WIS_hsa, na.rm = TRUE), 
            MWIS_rac = mean(WIS_rac), 
            MWIS_state = mean(WIS_state),
            MWIS_G_vs_county = mean(WIS_G_vs_county, na.rm = TRUE), 
            MWIS_dshs_region_vs_county = mean(WIS_dshs_region_vs_county, na.rm = TRUE), 
            MWIS_hsa_vs_county = mean(WIS_hsa_vs_county, na.rm = TRUE),
            MWIS_rac_vs_county = mean(WIS_rac_vs_county, na.rm = TRUE), 
            MWIS_state_vs_county = mean(WIS_state_vs_county, na.rm = TRUE))


sv_G <- sv2 %>%
  filter(season == "Overall",
         period_type == "Flu Season Months",
         weight_type == "population") %>%
  dplyr::select(K, geo_level, lambda_K) %>%
  bind_rows(
    tibble(
      K = c(1, 254),
      geo_level = c("state", "county"),
      lambda_K = c(0, 1)
    )
  )

wis_same <- all_wis2 %>%
  ungroup() %>%
  dplyr::select(n_cluster, horizon, starts_with("MWIS_"), -contains("_vs_county")) %>%
  pivot_longer(
    cols = starts_with("MWIS_"),
    names_to = "geo_level",
    values_to = "MWIS"
  ) %>%
  mutate(
    geo_level = str_remove(geo_level, "^MWIS_"),
    geo_level = recode(
      geo_level,
      "dshs_region" = "dshs",
      "G" = "cluster"
    ),
    K = case_when(
      geo_level == "cluster" ~ n_cluster,
      geo_level == "rac" ~ 22,
      geo_level == "dshs" ~ 8,
      geo_level == "hsa" ~ 61,
      geo_level == "state" ~ 1,
      geo_level == "county" ~ 254,
      TRUE ~ n_cluster
    )
  ) 

wis_vs_county <- all_wis2 %>%
  ungroup() %>%
 dplyr:: select(n_cluster, horizon, contains("_vs_county")) %>%
  pivot_longer(
    cols = contains("_vs_county"),
    names_to = "geo_level",
    values_to = "MWIS"
  ) %>%
  mutate(
    geo_level = str_remove(geo_level, "^MWIS_"),
    geo_level = str_remove(geo_level, "_vs_county$"),
    geo_level = recode(
      geo_level,
      "dshs_region" = "dshs",
      "G" = "cluster"
    ),
    K = case_when(
      geo_level == "cluster" ~ n_cluster,
      geo_level == "rac" ~ 22,
      geo_level == "dshs" ~ 8,
      geo_level == "hsa" ~ 61,
      geo_level == "state" ~ 1,
      geo_level == "county" ~ 254,
      TRUE ~ n_cluster
    )
  )


plot_df <- wis_vs_county %>% ## use either wis_same or wis_vs_county
  dplyr::select(n_cluster, horizon, geo_level, MWIS, K) %>%
  left_join(sv_G, by = c("K", "geo_level"), relationship = "many-to-many") %>%
  mutate(
    geo_level = factor(
      geo_level,
      levels = c("state", "dshs", "rac", "cluster", "hsa", "county")
    ),
    highlight = case_when(
      #geo_level == "state" & n_cluster == 5 ~ TRUE,
      geo_level == "dshs"  & n_cluster %in% c(7, 9) ~ TRUE,
      geo_level == "rac"   & n_cluster %in% c(21, 23) ~ TRUE,
      geo_level == "hsa"   & n_cluster == 61 ~ TRUE,
      geo_level == "cluster" & n_cluster %in% c(7, 9, 21, 23, 61) ~ TRUE,
      TRUE ~ FALSE
    )
  )

pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary/mwis_spatial_county.pdf",
    width = 10, height = 8)
ggplot(plot_df, aes(x = MWIS, y = lambda_K)) +
  geom_point(
    aes(color = factor(n_cluster), shape = geo_level),
    size = 2,
    alpha = 0.35
  ) +
  geom_point(
    data = plot_df %>% filter(highlight),
    aes(color = factor(n_cluster), shape = geo_level),
    size = 4,
    alpha = 1,
    #stroke = 1.5
  ) +
  facet_wrap(~ horizon) +
  theme_bw() +
  scale_shape_manual(
    values = c(
      state = 15,
      dshs = 18,
      rac = 17,
      cluster = 16,
      hsa = 3,
      county = 4
    )
  ) +
  labs(
    x = "MWIS",
    y = "lambda_K",
    color = "n_cluster",
    shape = "Geo level"
  )
dev.off()

##########################################################################
## population vs delta WIS
##########################################################################

geo_pop <- purrr::imap_dfr(
  res_all,
  ~ .x$obs_all %>%
    dplyr::select(location, population, geo_level) %>%
    dplyr::mutate(result_id = .y)
) %>%
  distinct()

county_pop <- geo_pop %>%
  filter(geo_level == "county") %>%
  dplyr::select(unit_id = location, population) %>%
  distinct()

all_wide_pop <- all_wide %>%
  left_join(county_pop, by = "unit_id")

all_long_pop <- all_wide_pop %>%
  dplyr::select(
    unit_id, target_end_date, reference_date, horizon,
    n_cluster, season, result_id, population,
    WIS_county,
    WIS_G,
    WIS_rac,
    WIS_dshs_region,
    WIS_hsa,
    WIS_state
  ) %>%
  pivot_longer(
    cols = c(
      WIS_county,
      WIS_G,
      WIS_rac,
      WIS_dshs_region,
      WIS_hsa,
      WIS_state
    ),
    names_to = "geo_level",
    values_to = "WIS"
  )

all_long_pop_summary <- all_long_pop %>%
  group_by(unit_id, population, geo_level, n_cluster, horizon) %>%
  summarise(
    mean_WIS = mean(WIS, na.rm = TRUE),
    median_WIS = median(WIS, na.rm = TRUE),
  )


pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary/pop_MWIS.pdf",
    width = 10, height = 8)
ggplot(all_long_pop_summary, aes(x = population, y = mean_WIS, color = geo_level)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  facet_wrap(~horizon) + 
  scale_x_log10()
dev.off()

##########################################################################
## population box plot by geo level
##########################################################################

all_geo_mapping <- purrr::imap_dfr(
  res_all,
  ~ .x$geo_mapping %>%
    dplyr::mutate(
      season = stringr::str_extract(.y, "\\d{4}$"),
      n_cluster = as.integer(stringr::str_extract(.y, "(?<=k)\\d+"))
    )
)

all_geo_mapping_pop <- all_geo_mapping %>% 
  left_join(county_pop, by = "unit_id")


all_geo_mapping_pop2 <- all_geo_mapping_pop %>%
  group_by(geo_level, location, season, n_cluster) %>%
  summarise(pop_G = sum(population))



cluster_levels <- paste0("cluster_", seq(5, 65, by = 2))

plot_levels <- c(
  "state",
  "cluster_5", "cluster_7",
  "dshs_region",
  paste0("cluster_", seq(9, 21, by = 2)),
  "rac",
  paste0("cluster_", seq(23, 61, by = 2)),
  "hsa",
  "cluster_63", "cluster_65",
  "county"
)

plot_df <- all_geo_mapping_pop2 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    plot_group = dplyr::case_when(
      geo_level == "cluster" ~ paste0("cluster_", n_cluster),
      TRUE ~ geo_level
    ),
    plot_group = factor(plot_group, levels = plot_levels)
  )

pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary/pop_boxplot.pdf",
    width = 10, height = 8)
ggplot(plot_df, aes(x = plot_group, y = pop_G, fill = geo_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  facet_wrap(~ season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "Population (log scale)",
    fill = "Geo level"
  )

dev.off()
##########################################################################
## population vs WIS
##########################################################################

wis_all <- purrr::imap_dfr(
  res_all,
  ~ .x$wis_all %>%
    dplyr::mutate(
      season = stringr::str_extract(.y, "\\d{4}$"),
      n_cluster = as.integer(stringr::str_extract(.y, "(?<=k)\\d+"))
    )
)

wis_long <- wis_all %>%
  pivot_longer(
    cols = c(
      WIS_G,
      WIS_dshs_region,
      WIS_rac,
      WIS_hsa,
      WIS_state,
      WIS_county
    ),
    names_to = "geo_level",
    values_to = "WIS"
  ) %>%
  mutate(
    geo_level = dplyr::case_when(
      geo_level == "WIS_G" ~ "cluster",
      geo_level == "WIS_dshs_region" ~ "dshs_region",
      geo_level == "WIS_rac" ~ "rac",
      geo_level == "WIS_hsa" ~ "hsa",
      geo_level == "WIS_state" ~ "state",
      geo_level == "WIS_county" ~ "county"
    ),
    plot_group = dplyr::case_when(
      geo_level == "cluster" ~ paste0("cluster_", n_cluster),
      TRUE ~ geo_level
    ),
    plot_group = factor(plot_group, levels = plot_levels)
  ) %>%
  dplyr::select(location, target_end_date, horizon, reference_date,
                 season, n_cluster, geo_level, WIS, plot_group)


wis_long_summary <- wis_long %>%
  group_by(horizon, season, location, geo_level, plot_group) %>%
  summarise(mean_WIS = mean(WIS))


pdf("/work2/09967/dongahkim0223/frontera/Spatial_clustering/figures/summary/MWIS_boxplot.pdf",
    width = 10, height = 8)
ggplot(wis_long_summary, aes(x = plot_group, y = mean_WIS, fill = geo_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_grid(season ~ horizon) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "WIS",
    fill = "Geo level"
  ) 


ggplot(wis_long_summary, aes(x = plot_group, y = mean_WIS, fill = geo_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~horizon) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "WIS",
    fill = "Geo level"
  ) 

ggplot(wis_long_summary, aes(x = plot_group, y = mean_WIS, fill = geo_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL,
    y = "WIS",
    fill = "Geo level"
  ) 
dev.off()
