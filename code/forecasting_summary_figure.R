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



library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
# -------------------------------------------------------------------------
# 1. COUNTY LEVEL 통합 테이블 생성
# -------------------------------------------------------------------------

# (1) 예측 성능 데이터 추출 (WIS)
county_wis <- purrr::map_dfr(res_county_clustergeo_list, "summary_metrics", .id = "result_id") %>%
  # 3-week horizon 조건 반영 (필요에 따라 horizon 값은 데이터에 맞게 조정: 예: 3 또는 "3 week")
  mutate(n_cluster = as.numeric(stringr::str_remove(result_id, "k"))) %>%
  # 필요한 컬럼만 선택 (각 레벨별 WIS 오차)
  dplyr::select(n_cluster, horizon, WIS_county, WIS_hsa, WIS_G, WIS_rac, WIS_dshs_region, WIS_state)

# (2) 지리적 변동 보존율 데이터 추출 (Spatial Variation)
county_spatial <- purrr::map_dfr(res_county_clustergeo_list, "spatial_var_metrics", .id = "result_id") %>%
  mutate(n_cluster = as.numeric(stringr::str_remove(result_id, "k"))) %>%
  group_by(n_cluster, geo_level, horizon) %>%
  summarise(mean_spatial_var = mean(spatial_variation_preserved, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = geo_level, values_from = mean_spatial_var, names_prefix = "SpatialVar_")

# (3) 두 데이터 결합하여 최종 County 테이블 완성
table_county_tradeoff <- county_wis %>%
  left_join(county_spatial, by = c("n_cluster", "horizon")) %>%
  arrange(n_cluster)

# 결과 확인
print("--- County Level Trade-off Table ---")
print(table_county_tradeoff)


p_all <- plot_tradeoff_matrix_facet(table_county_tradeoff, horizon_select = c(1, 2, 3, 4))
print(p_all)

# 격자가 크므로 파일로 저장해서 넓게 보시는 것을 추천합니다.
#ggsave("plots/tradeoff_matrix_all_horizons.png", plot = p_all, width = 12, height = 10, dpi = 300)




library(scales)


# 1. 3-week horizon 데이터 필터링 및 전처리
raw_filtered <- table_county_tradeoff %>%
  filter(horizon == 3)

unique_ks <- unique(raw_filtered$n_cluster)

# 2. Long format 변환 및 메트릭 분리
plot_df <- raw_filtered %>%
  pivot_longer(
    cols = starts_with("WIS_") | starts_with("SpatialVar_"),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  mutate(
    type = if_else(grepl("^WIS_", metric_raw), "WIS", "SpatialVar"),
    geo_level = sub("^(WIS_|SpatialVar_)", "", metric_raw)
  )

# 3. SpatialVar에 기준점 강제 주입 (State = 0, County = 1)
baseline_spatial <- tibble(
  n_cluster = rep(unique_ks, each = 2),
  horizon = 3,
  type = "SpatialVar",
  geo_level = rep(c("state", "county"), times = length(unique_ks)),
  value = rep(c(0, 1), times = length(unique_ks))
)

# 4. 데이터 최종 결합 및 요인(Factor) 순서 고정
plot_df_final <- plot_df %>%
  bind_rows(baseline_spatial) %>%
  mutate(
    # 교수님 지정 순서대로 범례 라벨 및 정렬 세팅
    geo_label = case_when(
      geo_level == "county"      ~ "County",
      geo_level == "hsa"         ~ "HSA",
      geo_level == "G"           ~ "Cluster (G)",
      geo_level == "rac"         ~ "RAC",
      geo_level == "dshs_region" ~ "DSHS Region",
      geo_level == "state"       ~ "State",
      TRUE ~ geo_level
    ),
    geo_label = factor(geo_label, levels = c("County", "HSA", "Cluster (G)", "RAC", "DSHS Region", "State")),
    
    # 패싯(Facet) 헤더에 예쁘게 표시될 이름 정의
    facet_label = case_when(
      type == "WIS" ~ "Forecasting Performance (WIS)",
      type == "SpatialVar" ~ "Spatial Variation Preserved (0 to 1)",
      TRUE ~ type
    ),
    facet_label = factor(facet_label, levels = c("Forecasting Performance (WIS)", "Spatial Variation Preserved (0 to 1)"))
  )

# 5. GGPLOT 패싯(Facet) 시각화 실행
ggplot(plot_df_final, aes(x = n_cluster, y = value, color = geo_label)) +
  geom_line(size = 1.1, alpha = 0.8) +
  geom_point(size = 2.5) + 
  
  # 핵심: scales = "free_y"를 주어 WIS 축과 SpatialVar 축이 독립적인 범위를 갖게 만듭니다.
  facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
  
  # 지리 레벨별 고정 색상 매핑
  scale_color_manual(
    name = "Geographic Level",
    values = c(
      "County"      = "#66a61e",  # 연두
      "HSA"         = "#e7298a",  # 핑크
      "Cluster (G)" = "#7570b3",  # 보라
      "RAC"         = "#1b9e77",  # 초록
      "DSHS Region" = "#d95f02",  # 주황
      "State"       = "#e6ab02"   # 노랑
    )
  ) +
  
  # 테마 레이아웃 및 폰트 세팅
  theme_bw(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"), # 패싯 헤더 배경 스타일
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  labs(
    title = "Evaluation Matrix: Forecast WIS vs Spatial Variation (3-Week Horizon)",
    x = "Number of Clusters (K)",
    y = "Metric Value"
  )


# -------------------------------------------------------------------------
# 2. HSA LEVEL 통합 테이블 생성 (HSA 분석 결과 리스트가 있다고 가정)
# -------------------------------------------------------------------------
# *주의: 만약 HSA 결과 리스트명이 다르면 'res_hsa_clustergeo_list' 부분을 수정하세요.
if (exists("res_hsa_clustergeo_list")) {
  
  hsa_wis <- purrr::map_dfr(res_hsa_clustergeo_list, "summary_metrics", .id = "result_id") %>%
    filter(horizon == 3) %>%
    mutate(n_cluster = as.numeric(stringr::str_remove(result_id, "k"))) %>%
    dplyr::select(n_cluster, WIS_hsa, WIS_G, WIS_state)
  
  hsa_spatial <- purrr::map_dfr(res_hsa_clustergeo_list, "spatial_var_metrics", .id = "result_id") %>%
    mutate(n_cluster = as.numeric(stringr::str_remove(result_id, "k"))) %>%
    group_by(n_cluster, geo_level) %>%
    summarise(mean_spatial_var = mean(spatial_variation_preserved, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = geo_level, values_from = mean_spatial_var, names_prefix = "SpatialVar_")
  
  table_hsa_tradeoff <- hsa_wis %>%
    left_join(hsa_spatial, by = "n_cluster") %>%
    arrange(n_cluster)
  
  print("--- HSA Level Trade-off Table ---")
  print(table_hsa_tradeoff)
}



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

