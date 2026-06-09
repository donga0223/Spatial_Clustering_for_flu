source("code/forecasting_evaluation_unit_functions.R")

date_list <- seq.Date(
  from = as.Date("2024-10-05"),
  to = as.Date("2025-03-01"),
  by = "week"
)

date_list <- seq.Date(
  from = as.Date("2025-10-04"),
  to = as.Date("2026-02-21"),
  by = "week"
)

unit_id_var <- "county"
unit_level_name <- "county"
unit_label <- "County"

# RAC 매핑 로드 (county level 분석에 필요)
rac_map <- read.csv("data/tx_rac.csv") %>%
  dplyr::select(RAC, County) %>%
  dplyr::rename(county = County) %>%
  dplyr::mutate(
    RAC    = as.character(RAC),
    county = as.character(county)
  )

res_skater_k5 <- run_cluster_eval(
  date_list = date_list,
  n_cluster = 5,
  method_name = "county_skater",
  unit_id_var = unit_id_var,
  unit_level_name = unit_level_name,
  unit_label = unit_label,
  rac_map = rac_map
)

res_skater_k5$summary_metrics
res_skater_k5$plots$h1


res_county_skater_list_2526 <- list()

for (k in 5:25) {
  print(k)
  res_county_skater_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_skater",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}


res_county_clustergeo_list_2526 <- list()

for (k in 5:25) {
  print(k)
  res_county_clustergeo_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_clustergeo",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}


res_county_redcap_list_2526 <- list()

for (k in 5:25) {
  print(k)
  res_county_redcap_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "county_redcap",
    unit_id_var = unit_id_var,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    rac_map = rac_map,
    make_plots = FALSE
  )
}

save(res_county_skater_list_2526, 
     res_county_clustergeo_list_2526, 
     res_county_redcap_list_2526, file = "wis_county_2526_all.RData")



summary_county_skater_all <- purrr::map_dfr(
  res_county_skater_list,
  "summary_metrics",
  .id = "result_id"
)

summary_county_clustergeo_all <- purrr::map_dfr(
  res_county_clustergeo_list,
  "summary_metrics",
  .id = "result_id"
)

summary_county_redcap_all <- purrr::map_dfr(
  res_county_redcap_list,
  "summary_metrics",
  .id = "result_id"
)



summary_county_skater_long2 <- make_summary_long(
  summary_county_skater_all,
  unit_level_name = unit_level_name,
  unit_label = unit_label
)

summary_county_clustergeo_long2 <- make_summary_long(
  summary_county_clustergeo_all,
  unit_level_name = unit_level_name,
  unit_label = unit_label
)

summary_county_redcap_long2 <- make_summary_long(
  summary_county_redcap_all,
  unit_level_name = unit_level_name,
  unit_label = unit_label
)



all_county_summary_long <- dplyr::bind_rows(
  summary_county_skater_long2,
  summary_county_clustergeo_long2,
  summary_county_redcap_long2,
  #summary_clustergeo0_long2
)

p_county_skater_summary <- plot_summary_metrics(
  summary_county_skater_all,
  method_name = "county_skater",
  unit_level_name = unit_level_name,
  unit_label = unit_label
)

p_county_clustergeo_summary <- plot_summary_metrics(
  summary_county_clustergeo_all,
  method_name = "county_clustergeo",
  unit_level_name = unit_level_name,
  unit_label = unit_label
)

p_county_redcap_summary <- plot_summary_metrics(
  summary_county_redcap_all,
  method_name = "county_redcap",
  unit_level_name = unit_level_name,
  unit_label = unit_label
)


p_county_coverage_compare <- plot_summary_method_type(
  all_county_summary_long,
  metric_type_select = "Coverage"
)

p_county_mae_compare <- plot_summary_method_type(
  all_county_summary_long,
  metric_type_select = "MAE"
)

p_county_wis_compare <- plot_summary_method_type(
  all_county_summary_long,
  metric_type_select = "WIS"
)


pdf("figures/county_forecasting_summary.pdf", width = 12, height = 8)

print(p_county_skater_summary)
print(p_county_clustergeo_summary)
print(p_county_redcap_summary)
print(p_county_coverage_compare)
print(p_county_mae_compare)
print(p_county_wis_compare)

dev.off()


#######3
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

make_location_mwis_all_county <- function(res_list, method_name = NULL) {
  
  purrr::imap_dfr(res_list, function(res, id) {
    
    k <- as.integer(stringr::str_remove(id, "k"))
    
    method_use <- ifelse(
      is.null(method_name),
      res$method_name,
      method_name
    )
    
    res$wis_all %>%
      dplyr::group_by(location, horizon) %>%
      dplyr::summarise(
        MWIS_county = mean(WIS_county, na.rm = TRUE),
        MWIS_rac_vs_county = mean(WIS_rac_vs_county, na.rm = TRUE),
        MWIS_G_vs_county = mean(WIS_G_vs_county, na.rm = TRUE),
        MWIS_state_vs_county = mean(WIS_state_vs_county, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        n_cluster = k,
        method_name = method_use,
        
        delta_mwis_g_county =
          MWIS_G_vs_county - MWIS_county,
        
        delta_mwis_rac_county =
          MWIS_rac_vs_county - MWIS_county,
        
        delta_mwis_state_county =
          MWIS_state_vs_county - MWIS_county
      )
  })
}

location_mwis_all_county <- dplyr::bind_rows(
  make_location_mwis_all_county(res_county_skater_list),
  make_location_mwis_all_county(res_county_clustergeo_list),
  make_location_mwis_all_county(res_county_redcap_list)
)

county_pop <- res_county_skater_list$k20$obs %>%
  dplyr::select(county, population) %>%
  distinct() %>%
  mutate(pop_quantile = dplyr::ntile(population, 4)) %>%
  mutate(pop_group = factor(
    pop_quantile,
    #labels = c("Small", "Large")
    labels = c("Q1", "Q2", "Q3", "Q4")
  ))

location_mwis_all_county_pop <- location_mwis_all_county %>%
  left_join(county_pop, by = c("location" = "county")) %>%
  filter(horizon <=4 )

plot_fraction_mwis_better <- function(location_mwis_all,
                                      unit_level_name = "hsa",
                                      unit_label = "HSA",
                                      include_rac = TRUE) {
  
  delta_cluster <- paste0("delta_mwis_g_", unit_level_name)
  delta_state   <- paste0("delta_mwis_state_", unit_level_name)
  delta_rac     <- paste0("delta_mwis_rac_", unit_level_name)
  
  frac_better <- location_mwis_all %>%
    dplyr::group_by(method_name, n_cluster, horizon, pop_group) %>%
    dplyr::summarise(
      frac_cluster_better = mean(.data[[delta_cluster]] < 0, na.rm = TRUE),
      frac_state_better   = mean(.data[[delta_state]] < 0, na.rm = TRUE),
      frac_rac_better     = if (include_rac) mean(.data[[delta_rac]] < 0, na.rm = TRUE) else NA_real_,
      n_locations = dplyr::n_distinct(location),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        if (include_rac) {
          c("frac_cluster_better", "frac_rac_better", "frac_state_better")
        } else {
          c("frac_cluster_better", "frac_state_better")
        }
      ),
      names_to = "comparison",
      values_to = "fraction_better"
    ) %>%
    dplyr::mutate(
      comparison = dplyr::case_when(
        comparison == "frac_cluster_better" ~ paste0("Cluster MWIS < ", unit_label, " MWIS"),
        comparison == "frac_rac_better"     ~ paste0("RAC MWIS < ", unit_label, " MWIS"),
        comparison == "frac_state_better"   ~ paste0("State MWIS < ", unit_label, " MWIS"),
        TRUE ~ comparison
      )
    )
  
  ggplot2::ggplot(
    frac_better,
    ggplot2::aes(
      x = n_cluster,
      y = fraction_better,
      color = comparison,
      alpha = pop_group,
      group = interaction(comparison, pop_group)
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_alpha_manual(values = c("Q1" = 0.2, "Q2" = 0.4, "Q3" = 0.7, "Q4" = 1)) +
    scale_color_manual(values = as.vector(cols25(4)) ) +
    ggplot2::facet_grid(method_name ~ horizon) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = paste0("Fraction of ", unit_label, "s"),
      color = "Comparison",
      title = paste0("Fraction of ", unit_label, "s where aggregate MWIS beats ", unit_label, " MWIS")
    )
}

p_county <- plot_fraction_mwis_better(
  location_mwis_all_county_pop,
  unit_level_name = "county",
  unit_label = "County"
)

pdf("figures/p_frac_pop_mwis_county.pdf", width = 12, height = 8)
p_county
dev.off()





rep_counties <- c(
  "Potter", "Lubbock", "Wichita", "Taylor", "Dallas", "Tarrant", "Bowie", 
  "Gregg", "Angelina", "El Paso", "Ector", "Midland", "Tom Green", "Bell", 
  "McLennan", "Brazos", "Travis", "Bexar", "Harris", "Galveston", "Victoria", 
  "Webb", "Nueces", "Hidalgo", "Cameron"
  )

plot_input <- res_skater_k5$df_all_wide2 %>%
  filter(unit_id %in% rep_counties)

plots <- make_horizon_plots(
  df_all_wide = plot_input,
  date_list = date_list,
  method_name = "skater",
  n_cluster = 5,
  unit_level_name = "county",
  unit_label = "County"
)

pdf("figures/county_skater5_rac.pdf", width = 12, height = 8)
print(plots)
dev.off()


head(res_county_skater_list$k5$wis_all)

res_county_skater_list$k5$obs_all %>% 
  dplyr::select(location, population) %>%
  distinct() %>%
  #arrange(desc(population)) %>%
  arrange(population) %>%
  slice_head(n = 20)

county_skater5_h <- make_horizon_plots(df_all_wide = res_county_skater_list$k20$df_all_wide %>%
                     filter(unit_id %in% c("Harris", "Dallas", "Tarrant", "Bexar", "Travis",
                                           "Kenedy", "Loving", "King", "McMullen", "Borden")),
                   date_list = date_list,
                   method_name = "skater",
                   n_cluster = 5,
                   unit_level_name = "county",
                   unit_label = "County")
pdf("figures/county_skater20_h.pdf", width = 12, height = 8)
print(county_skater5_h)
dev.off()



head(res_county_skater_list$k20$wis_all)


wis_county_skater_all <- purrr::map_dfr(
  res_county_skater_list,
  "wis_all",
  .id = "result_id"
)

library(tidyverse)

res_county_skater_list$k20$wis_all %>%
  filter(horizon == 1) %>%
  pivot_longer(
    cols = c(
      WIS_county,
      WIS_G,
      WIS_rac,
      WIS_state,
      WIS_G_vs_county,
      WIS_rac_vs_county,
      WIS_state_vs_county
    ),
    names_to = "metric",
    values_to = "WIS"
  ) %>%
  ggplot(aes(x = location, y = WIS, color = metric)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~metric) + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom"
  )

head(res_county_skater_list$k5$df_all)



