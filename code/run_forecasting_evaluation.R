source("code/forecasting_evaluation_functions.R")

date_list1 <- seq.Date(
  from = as.Date("2024-10-05"),
  to = as.Date("2025-03-30"),
  by = "week"
)

date_list <- seq.Date(
  from = as.Date("2025-10-04"),
  to = as.Date("2026-02-21"),
  by = "week"
)



res_skater_k2 <- run_cluster_eval(
  date_list = date_list,
  n_cluster = 2,
  method_name = "skater"
)
res_skater_k2$summary_metrics
res_skater_k2$plots


res_clustergeo_k2 <- run_cluster_eval(
  date_list = date_list,
  n_cluster = 2,
  method_name = "clustergeo"
)
res_clustergeo_k2$summary_metrics




res_skater_list_2526 <- list()

for (k in 2:20) {
  print(k)
  res_skater_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "skater",
    make_plots = TRUE
  )
}

res_clustergeo_list_2526 <- list()

for (k in 2:20) {
  print(k)
  res_clustergeo_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "clustergeo",
    make_plots = TRUE
  )
}

res_redcap_list_2526 <- list()

for (k in 2:20) {
  print(k)
  res_redcap_list_2526[[paste0("k", k)]] <- run_cluster_eval(
    date_list = date_list,
    n_cluster = k,
    method_name = "redcap",
    make_plots = TRUE
  )
}

#save(res_skater_list, res_clustergeo_list, res_redcap_list, file = "wis_all.RData")
save(res_skater_list_2526, res_clustergeo_list_2526, res_redcap_list_2526, file = "wis_2526_all.RData")

load("wis_2526_all.RData")

#res_clustergeo0_list <- list()

#for (k in 2:20) {
#  print(k)
#  res_clustergeo0_list[[paste0("k", k)]] <- run_cluster_eval(
#    date_list = date_list,
#    n_cluster = k,
#    method_name = "clustergeo0",
#    make_plots = TRUE
#  )
#}

res_skater_list <- res_skater_list_2526
res_clustergeo_list <- res_clustergeo_list_2526
res_redcap_list <- res_redcap_list_2526


summary_skater_all <- purrr::map_dfr(
  res_skater_list,
  "summary_metrics",
  .id = "result_id"
)


summary_clustergeo_all <- purrr::map_dfr(
  res_clustergeo_list,
  "summary_metrics",
  .id = "result_id"
)

summary_redcap_all <- purrr::map_dfr(
  res_redcap_list,
  "summary_metrics",
  .id = "result_id"
)

summary_clustergeo0_all <- purrr::map_dfr(
  res_clustergeo0_list,
  "summary_metrics",
  .id = "result_id"
)

summary_skater_long2 <- make_summary_long(summary_skater_all)
summary_clustergeo_long2 <- make_summary_long(summary_clustergeo_all)
summary_redcap_long2 <- make_summary_long(summary_redcap_all)
summary_clustergeo0_long2 <- make_summary_long(summary_clustergeo0_all)

all_summary_long <- rbind(summary_skater_long2, 
                     summary_clustergeo_long2,
                     summary_redcap_long2)
                     #summary_clustergeo0_long2)

p_skater_summary <- plot_summary_metrics(summary_skater_all, method_name = "skater")
p_clustergeo_summary <- plot_summary_metrics(summary_clustergeo_all, method_name = "clustergeo")
p_redcap_summary <- plot_summary_metrics(summary_redcap_all, method_name = "redcap")
p_clustergeo0_summary <- plot_summary_metrics(summary_clustergeo0_all, method_name = "clustergeo0")

p_coverage_compare <- plot_summary_method_type(all_summary_long,  metric_type_select = "Coverage")
p_mae_compare <- plot_summary_method_type(all_summary_long,  metric_type_select = "MAE")
p_wis_compare <- plot_summary_method_type(all_summary_long,  metric_type_select = "WIS")

pdf("figures/forecasting_2526_summary.pdf", width = 12, height = 8)
p_skater_summary
p_clustergeo_summary
p_redcap_summary
p_clustergeo0_summary
p_coverage_compare
p_mae_compare
p_wis_compare
dev.off()

pdf("figures/skater5_2526_h.pdf", width = 12, height = 8)
res_skater_list$k5$plots
dev.off()
pdf("figures/skater20_2526_h.pdf", width = 12, height = 8)
res_skater_list$k20$plots
dev.off()


res_skater_list$k20$wis_all %>%
  filter(horizon == 1) %>%
  pivot_longer(
    cols = c(
      WIS_hsa,
      WIS_G,
      WIS_state,
      WIS_G_vs_hsa,
      WIS_state_vs_hsa
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


#######3
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

make_location_mwis_all <- function(res_list, method_name = NULL) {
  
  purrr::imap_dfr(res_list, function(res, id) {
    k <- as.integer(stringr::str_remove(id, "k"))
    
    method_use <- ifelse(is.null(method_name), res$method_name, method_name)
    
    res$wis_all %>%
      dplyr::group_by(location, horizon) %>%
      dplyr::summarise(
        MWIS_hsa = mean(WIS_hsa, na.rm = TRUE),
        MWIS_G_vs_hsa = mean(WIS_G_vs_hsa, na.rm = TRUE),
        MWIS_state_vs_hsa = mean(WIS_state_vs_hsa, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        n_cluster = k,
        method_name = method_use,
        delta_mwis_g_hsa = MWIS_G_vs_hsa - MWIS_hsa,
        delta_mwis_state_hsa = MWIS_state_vs_hsa - MWIS_hsa
      )
  })
}

location_mwis_all <- dplyr::bind_rows(
  make_location_mwis_all(res_skater_list),
  make_location_mwis_all(res_clustergeo_list),
  make_location_mwis_all(res_redcap_list)
)

hsa_pop <- res_skater_list$k20$obs %>%
  dplyr::select(hsa_nci_id, hsa_population) %>%
  mutate(hsa_nci_id = as.character(hsa_nci_id)) %>%
  distinct() %>%
  mutate(pop_quantile = dplyr::ntile(hsa_population, 4)) %>%
  mutate(pop_group = factor(
    pop_quantile,
    #labels = c("Small", "Large")
    labels = c("Q1", "Q2", "Q3", "Q4")
  ))
  

location_mwis_all_pop <- location_mwis_all %>%
  left_join(hsa_pop, by = c("location" = "hsa_nci_id")) %>%
  filter(horizon <=4 )

plot_fraction_mwis_better <- function(location_mwis_all,
                                      unit_label = "HSA") {
  
  frac_better <- location_mwis_all %>%
    dplyr::group_by(method_name, n_cluster, horizon, pop_group) %>%
    dplyr::summarise(
      frac_cluster_better = mean(delta_mwis_g_hsa < 0, na.rm = TRUE),
      frac_state_better = mean(delta_mwis_state_hsa < 0, na.rm = TRUE),
      n_locations = dplyr::n_distinct(location),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(frac_cluster_better, frac_state_better),
      names_to = "comparison",
      values_to = "fraction_better"
    ) %>%
    dplyr::mutate(
      comparison = dplyr::case_when(
        comparison == "frac_cluster_better" ~ paste0("Cluster MWIS < ", unit_label, " MWIS"),
        comparison == "frac_state_better" ~ paste0("State MWIS < ", unit_label, " MWIS"),
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
    #ggplot2::scale_alpha_manual(values = c("Small" = 0.4, "Large" = 1)) +
    ggplot2::scale_alpha_manual(values = c("Q1" = 0.2, "Q2" = 0.4, "Q3" = 0.7, "Q4" = 1)) +
    scale_color_manual(values = as.vector(cols25(4)) ) +
    ggplot2::facet_grid(method_name ~ horizon) +
    #ggplot2::facet_grid(method_name ~ horizon) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::labs(
      x = "Number of clusters",
      y = paste0("Fraction of ", unit_label, "s"),
      color = "Comparison",
      title = paste0("Fraction of Clusters/State where aggregate MWIS beats ", unit_label, " MWIS")
    )
}

p_frac_mwis <- plot_fraction_mwis_better(location_mwis_all_pop, unit_label = "HSA")
pdf("figures/p_frac_pop_mwis.pdf", width = 12, height = 8)
p_frac_mwis
dev.off()


location_mwis_all_pop %>%
  ggplot(aes(x=log(hsa_population))) +
  geom_point(aes(y = delta_mwis_g_hsa, colour = "#1F78C8")) +
  geom_point(aes(y = delta_mwis_state_hsa, colour = "#ff0000")) +
  facet_grid(method_name ~ horizon)

## the code is already changed including pop_group, so will not work for location_nwis_all data
#p_frac_mwis <- plot_fraction_mwis_better(location_mwis_all, unit_label = "HSA")
#pdf("figures/p_frac_mwis.pdf", width = 12, height = 8)
#p_frac_mwis
#dev.off()




location_similarity <- location_wis_all %>%
  dplyr::group_by(method_name, horizon, location) %>%
  dplyr::summarise(
    mean_abs_delta_wis = mean(delta_wis_hsa, na.rm = TRUE),
    mean_delta_wis = mean(delta_wis_hsa, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(location_similarity,
       aes(x = mean_delta_wis,
           y = reorder(location, mean_abs_delta_wis))) +
  geom_point() +
  facet_wrap(~ horizon, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Mean |Cluster WIS - HSA WIS|",
    y = "HSA location",
    title = "How similar are cluster forecasts to HSA forecasts?"
  )


location_heat_hsa <- location_wis_all %>%
  dplyr::group_by(method_name, horizon, location, n_cluster) %>%
  dplyr::summarise(
    mean_abs_delta_wis = mean(abs_delta_wis_hsa, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(location_heat_hsa,
       aes(x = n_cluster, y = location, fill = log10(mean_abs_delta_wis + 1e-6))) +
  geom_tile() +
  facet_wrap(~ horizon, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Number of clusters",
    y = "HSA location",
    fill = "log10(|ΔWIS|)",
    title = "Location-level similarity between cluster and HSA forecasts"
  )


top_noisy <- location_similarity %>%
  group_by(horizon) %>%
  slice_max(mean_abs_delta_wis, n = 20) %>%
  ungroup()

ggplot(top_noisy,
       aes(x = mean_abs_delta_wis,
           y = reorder(location, mean_abs_delta_wis))) +
  geom_point() +
  facet_wrap(~ horizon, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Mean |Cluster WIS - HSA WIS|",
    y = "HSA location",
    title = "Top 20 locations least similar to HSA forecasts"
  )
