library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)



make_summary_long <- function(summary_all,
                              unit_level_name = "hsa",
                              unit_label = "HSA",
                              agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  agg_levels <- agg_levels[agg_levels != unit_level_name]
  
  level_labels <- c(
    G = "Cluster",
    rac = "RAC",
    dshs_region = "DSHS Region",
    hsa = "HSA",
    county = "County",
    state = "State"
  )
  
  level_labels[unit_level_name] <- unit_label
  
  metric_cols <- names(summary_all)[
    grepl("^(coverage|MAE|WIS|spatial_var)_", names(summary_all))
  ]
  
  summary_all %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::case_when(
        grepl("^coverage", metric) ~ "Coverage",
        grepl("^MAE", metric) ~ "MAE",
        grepl("^WIS", metric) ~ "WIS",
        grepl("^spatial_var", metric) ~ "Spatial variation",
        TRUE ~ metric
      ),
      
      level = dplyr::case_when(
        metric == paste0("coverage_", unit_level_name) |
          metric == paste0("MAE_", unit_level_name) |
          metric == paste0("WIS_", unit_level_name) ~ unit_level_name,
        
        grepl("_vs_", metric) ~ stringr::str_match(metric, "^(coverage|MAE|WIS)_(.*)_vs_")[, 3],
        
        grepl("^spatial_var_", metric) ~ stringr::str_remove(metric, "^spatial_var_"),
        
        TRUE ~ stringr::str_remove(metric, "^(coverage|MAE|WIS)_")
      ),
      
      metric_clean = dplyr::case_when(
        level == unit_level_name & !grepl("_vs_", metric) ~
          paste0(unit_label, " vs ", unit_label),
        
        grepl("_vs_", metric) ~
          paste0(level_labels[level], " forecast vs ", unit_label, " obs"),
        
        grepl("^spatial_var_", metric) ~
          paste0(level_labels[level], " spatial variation"),
        
        TRUE ~
          paste0(level_labels[level], " vs ", level_labels[level])
      ),
      
      metric_type = factor(
        metric_type,
        levels = c("Coverage", "MAE", "WIS", "Spatial variation")
      )
    )
}

plot_summary_metrics <- function(summary_all,
                                 method_name = NULL,
                                 unit_level_name = "hsa",
                                 unit_label = "HSA",
                                 agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  )
  
  p <- ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = metric_clean,
      group = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      metric_type ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Value",
      color = "Comparison",
      title = paste("Method:", method_name)
    )
  
  return(p)
}


plot_summary_metrics_same_level <- function(summary_all,
                                            method_name = NULL,
                                            unit_level_name = "county",
                                            unit_label = "County",
                                            agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  ) |>
    dplyr::filter(
      !stringr::str_detect(metric, paste0("_vs_", unit_level_name)),
      !stringr::str_detect(metric, "spatial_var")
    )
  
  level_order_same <- c(
    "County vs County",
    "HSA vs HSA", 
    "Cluster vs Cluster",
    "RAC vs RAC",                
    "DSHS Region vs DSHS Region",
    "State vs State"   
  )
  
  summary_long2 <- summary_long2 |>
    dplyr::mutate(
      metric_clean = factor(
        metric_clean,
        levels = level_order_same
      )
    )
  ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = metric_clean,
      group = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      metric_type ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Value",
      color = "Comparison",
      title = paste("Same-level evaluation - Method:", method_name)
    )
}

plot_summary_metrics_vs_unit <- function(summary_all,
                                         method_name = NULL,
                                         unit_level_name = "county",
                                         unit_label = "County",
                                         agg_levels = c("G", "rac", "dshs_region", "hsa", "state")) {
  
  summary_long2 <- make_summary_long(
    summary_all = summary_all,
    unit_level_name = unit_level_name,
    unit_label = unit_label,
    agg_levels = agg_levels
  ) |>
    dplyr::filter(
      stringr::str_detect(metric, paste0("_", unit_level_name, "$")) |
        stringr::str_detect(metric, paste0("_vs_", unit_level_name, "$"))
    ) |>
    dplyr::filter(
      !stringr::str_detect(metric, "spatial_var")
    )
  
  level_order_vs <- c(
    "County vs County",
    "HSA forecast vs County obs",
    "Cluster forecast vs County obs",
    "RAC forecast vs County obs",
    "DSHS Region forecast vs County obs",
    "State forecast vs County obs"      
  )
  summary_long2 <- summary_long2 |>
    dplyr::mutate(
      metric_clean = factor(
        metric_clean,
        levels = level_order_vs
      )
    )
  
  ggplot2::ggplot(
    summary_long2,
    ggplot2::aes(
      x = n_cluster,
      y = value,
      color = metric_clean,
      group = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_grid(
      metric_type ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Value",
      color = "Comparison",
      title = paste("Evaluation against", unit_label, "observations - Method:", method_name)
    )
}

plot_spatial_variation_vs_state <- function(spatial_var,
                                            method_name = NULL) {
  
  level_order <- c(
    "County vs HSA",
    "County vs Cluster",
    "County vs RAC",
    "County vs DSHS Region"
  )
  
  spatial_long <- spatial_var |>
    dplyr::mutate(
      n_cluster = as.numeric(stringr::str_remove(result_id, "k")),
      metric_clean = dplyr::case_when(
        geo_level == "hsa" ~ "County vs HSA",
        geo_level == "G" ~ "County vs Cluster",
        geo_level == "rac" ~ "County vs RAC",
        geo_level == "dshs_region" ~ "County vs DSHS Region",
        TRUE ~ geo_level
      ),
      metric_clean = factor(metric_clean, levels = level_order)
    )
  
  spatial_summary <- spatial_long %>%
    group_by(n_cluster, metric_clean, horizon) %>%
    summarise(mean_preserved = mean(spatial_variation_preserved, na.rm = TRUE))
  
  ggplot2::ggplot(
    spatial_summary,
    ggplot2::aes(
      x = n_cluster,
      y = mean_preserved,
      color = metric_clean,
      group = metric_clean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylim(0,NA) +
    ggplot2::facet_wrap(
      ~ horizon,
      scales = "free_y"
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Number of clusters",
      y = "Spatial variation preserved",
      color = "Comparison",
      title = paste("Spatial variation preserved vs State - Method:", method_name)
    )
}


plot_tradeoff_matrix_facet_fixed <- function(tradeoff_df, horizon_select = c(1, 2, 3, 4)) {
  
  # 1. 원하는 주차(Horizon) 데이터 필터링
  raw_filtered <- tradeoff_df %>%
    filter(horizon %in% horizon_select)
  
  unique_ks <- unique(raw_filtered$n_cluster)
  unique_horizons <- unique(raw_filtered$horizon)
  
  # 2. Long format 변환 (★ 버그 수정: | 대신 c()를 사용하여 모든 R 버전 호환 완료)
  plot_df <- raw_filtered %>%
    pivot_longer(
      cols = c(starts_with("WIS_"), starts_with("SpatialVar_")),
      names_to = "metric_raw",
      values_to = "value"
    ) %>%
    mutate(
      type = if_else(grepl("^WIS_", metric_raw), "WIS", "SpatialVar"),
      geo_level = sub("^(WIS_|SpatialVar_)", "", metric_raw)
    )
  
  # 3. SpatialVar 에 없는 State(0)와 County(1) 기준 데이터 강제 주입
  baseline_spatial <- expand.grid(
    n_cluster = unique_ks,
    horizon = unique_horizons,
    geo_level = c("state", "county"),
    type = "SpatialVar",
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      value = if_else(geo_level == "state", 0, 1),
      # ★ 형식을 plot_df와 완벽히 맞추기 위해 metric_raw 컬럼을 강제로 만들어 줍니다.
      metric_raw = paste0("SpatialVar_", geo_level) 
    )
  
  # 4. 데이터 최종 결합 및 요인(Factor) 순서 고정
  plot_df_final <- plot_df %>%
    bind_rows(baseline_spatial) %>%
    mutate(
      # 지리 레벨 라벨 정렬 및 이름 매핑 (County -> HSA -> Cluster -> RAC -> DSHS -> State 순서)
      geo_label = case_when(
        geo_level == "county"      ~ "County",
        geo_level == "hsa"         ~ "HSA",
        geo_level == "rac"         ~ "RAC",
        geo_level == "G"           ~ "Cluster (G)",
        geo_level == "dshs_region" ~ "DSHS Region",
        geo_level == "state"       ~ "State",
        TRUE ~ geo_level
      ),
      geo_label = factor(geo_label, levels = c("County", "HSA", "RAC", "Cluster (G)", "DSHS Region", "State")),
      
      # 가로축 패싯 네임 설정
      facet_metric = case_when(
        type == "WIS" ~ "Forecasting Performance (WIS)",
        type == "SpatialVar" ~ "Spatial Variation Preserved (0 to 1)",
        TRUE ~ type
      ),
      facet_metric = factor(facet_metric, levels = c("Forecasting Performance (WIS)", "Spatial Variation Preserved (0 to 1)")),
      
      # 세로축 패싯 네임 설정
      facet_horizon = factor(paste0("Horizon ", horizon), levels = paste0("Horizon ", sort(horizon_select)))
    ) %>%
    filter(!is.na(value), !is.na(geo_label))
  
  # 5. GGPLOT 패싯 격자 그리기
  p <- ggplot(plot_df_final, aes(x = n_cluster, y = value, color = geo_label, group = geo_label)) +
    geom_line(size = 1.1, alpha = 0.8) +
    geom_point(size = 2.0) + 
    
    # 가로는 메트릭(WIS vs SpatialVar), 세로는 주차(Horizon 1~4)
    facet_grid(facet_metric ~ facet_horizon, scales = "free_y") +
    
    # 지정해주신 순서에 맞는 색상 지정
    scale_color_manual(
      name = "Geographic Level",
      values = c(
        "County"      = "#66a61e",  # 연두
        "HSA"         = "#e7298a",  # 핑크
        "Cluster (G)" = "#7570b3",  # 보라
        "RAC"         = "#1b9e77",  # 초록
        "DSHS Region" = "#d95f02",  # 주황
        "State"       = "#e6ab02"   # 노랑
      ),
      drop = FALSE
    ) +
    
    theme_bw(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#f7f7f7", color = "#cccccc"),
      strip.text = element_text(face = "bold", size = 11),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title.x = element_text(margin = margin(t = 10))
    ) +
    labs(
      title = "Multi-Horizon Trade-off Matrix (Facet Grid)",
      x = "Number of Clusters (K)",
      y = "Value"
    )
  
  return(p)
}