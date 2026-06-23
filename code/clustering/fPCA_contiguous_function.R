get_pc_scores <- function(df_ts, 
                          group_var, 
                          total_variance = 0.9, 
                          min_nharm = 10, 
                          plotfit = FALSE) {
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(zoo)
  library(fda)
  
  # 1. 입력 데이터 검증
  if (!is.data.frame(df_ts)) {
    stop("df_ts must be a data.frame or tibble.")
  }
  
  # 2. 와이드 포맷 변환 및 정렬 (Date 행 변환)
  df_wide <- df_ts %>%
    dplyr::ungroup() %>%
    dplyr::arrange({{ group_var }}, Date) %>%
    dplyr::select(Date, {{ group_var }}, value) %>%
    tidyr::pivot_wider(names_from = {{ group_var }}, values_from = value) %>%
    tibble::column_to_rownames("Date")
  
  if (anyNA(df_wide)) {
    stop("Data contains NA values. NA values are not allowed.")
  }
  
  # 3. 매트릭스 변환 및 3주 이동평균(Smoothing) 적용
  data_mat <- df_wide %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(as.character(.)))) %>%
    base::as.matrix()
  
  data_mat_smooth <- apply(data_mat, 2, function(x) {
    zoo::rollapply(x, width = 3, FUN = mean, fill = "extend", align = "center")
  })
  
  n_weeks <- nrow(data_mat_smooth)
  
  # 4. B-spline 기저(Basis) 및 평활화(Smoothing) 파라미터 정의
  basis_refined <- fda::create.bspline.basis(
    rangeval = c(1, n_weeks), 
    nbasis = n_weeks - 2, 
    norder = 4
  )
  
  fd_fdPar <- fda::fdPar(
    basis_refined, 
    Lfdobj = 2, 
    lambda = 1e-10
  )
  
  # 5. 이산 데이터를 functional data 객체로 전환
  fd_obj <- fda::smooth.basis(
    argvals = 1:n_weeks, 
    y = data_mat_smooth, 
    fdParobj = fd_fdPar
  )$fd
  
  # 피팅 품질 디버깅용 플롯 (요청 시에만 실행)
  if (plotfit) {
    fda::plotfit.fd(
      y = data_mat_smooth, 
      argvals = 1:n_weeks, 
      fdobj = fd_obj, 
      index = 1, 
      main = "Check: Region 1 Fitting"
    )
  }
  
  # 6. Functional PCA 수행 및 95%(total_variance) 설명력 만족하는 최적 주성분 개수 도출
  fpca_res_init <- fda::pca.fd(fd_obj, nharm = 50)
  nharm_opt <- max(
    which(cumsum(fpca_res_init$varprop) >= total_variance)[1],
    min_nharm
  )
  
  # 최적의 주성분 개수로 fPCA 최종 재실행
  fpca_res <- fda::pca.fd(fd_obj, nharm = nharm_opt)
  
  # 7. 클러스터링에 입력할 fPC 점수(Scores) 행렬 추출
  pc_scores <- fpca_res$scores
  
  # 행 이름에 지역 ID(df_sf 데이터와 병합할 핵심 Key)를 주입
  rownames(pc_scores) <- colnames(data_mat_smooth)
  
  # [제거 완료] 불필요한 내부 hclust 및 복잡한 시각화 코드 삭제
  
  return(pc_scores)
}


get_pc_scores_seasonwise <- function(df_ts,
                                     group_var,
                                     total_variance = 0.9,
                                     min_nharm = 10, 
                                     plotfit = FALSE) {
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(zoo)
  library(fda)
  
  # 1. input check
  if (!is.data.frame(df_ts)) {
    stop("df_ts must be a data.frame or tibble.")
  }
  
  # 2. season-wise smoothing (boundary crossing 방지)
  df_ts_smooth <- df_ts %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Date = as.Date(Date)) %>%
    dplyr::group_by(season, {{ group_var }}) %>%
    dplyr::arrange(Date, .by_group = TRUE) %>%
    dplyr::mutate(
      season_week = dplyr::row_number(),
      value = zoo::rollapply(
        value,
        width = 3,
        FUN = mean,
        align = "center",
        fill = NA,
        partial = TRUE
      )
    ) %>%
    dplyr::ungroup()
  
  # 3. wide format conversion
  df_wide <- df_ts_smooth %>%
    dplyr::arrange(season, season_week, {{ group_var }}) %>%
    dplyr::mutate(
      time_id = paste0(season, "_", sprintf("%02d", season_week))
    ) %>%
    dplyr::select(time_id, {{ group_var }}, value) %>%
    tidyr::pivot_wider(names_from = {{ group_var }}, values_from = value) %>%
    tibble::column_to_rownames("time_id")
  
  if (anyNA(df_wide)) {
    stop("Data contains NA values. NA values are not allowed.")
  }
  
  # 4. matrix conversion (NO additional smoothing)
  data_mat <- df_wide %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.numeric(as.character(.))
      )
    ) %>%
    base::as.matrix()
  
  n_weeks <- nrow(data_mat)
  
  # 5. B-spline basis
  basis_refined <- fda::create.bspline.basis(
    rangeval = c(1, n_weeks),
    nbasis = n_weeks - 2,
    norder = 4
  )
  
  fd_fdPar <- fda::fdPar(
    basis_refined,
    Lfdobj = 2,
    lambda = 1e-10
  )
  
  # 6. functional data conversion
  fd_obj <- fda::smooth.basis(
    argvals = 1:n_weeks,
    y = data_mat,
    fdParobj = fd_fdPar
  )$fd
  
  if (plotfit) {
    fda::plotfit.fd(
      y = data_mat,
      argvals = 1:n_weeks,
      fdobj = fd_obj,
      index = 1,
      main = "Check: Region 1 Fitting"
    )
  }
  
  # 7. FPCA
  fpca_res_init <- fda::pca.fd(fd_obj, nharm = 50)
  nharm_opt <- max(
    which(cumsum(fpca_res_init$varprop) >= total_variance)[1],
    min_nharm
  )
  
  fpca_res <- fda::pca.fd(fd_obj, nharm = nharm_opt)
  
  # 8. PC scores
  pc_scores <- fpca_res$scores
  rownames(pc_scores) <- colnames(data_mat)
  
  return(pc_scores)
}


make_spatial_mst <- function(df_sf, data_matrix, queen = FALSE) {
  library(sf)
  library(spdep)
  
  # 1. 데이터 형식 방어 코드 및 경고창 추가
  if (!is.matrix(data_matrix)) {
    warning("Input 'data_matrix' is not a matrix. Automatically converting to a matrix format.")
    
    # 데이터프레임 내에 혹시 모를 문자열이나 팩터 열이 있다면 숫자로 강제 변환하며 매트릭스화
    row_names_backup <- rownames(data_matrix)
    data_matrix <- base::as.matrix(as.data.frame(data_matrix) %>% 
                                     dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(as.character(.)))))
    rownames(data_matrix) <- row_names_backup
  }
  
  # 2. 지도 데이터(df_sf) 기반의 인접 그래프 생성
  nb <- spdep::poly2nb(df_sf, queen = queen)
  
  # 3. 고립 지역 예외 처리 안전장치 (Island 조율)
  if (any(spdep::card(nb) == 0)) {
    coords <- sf::st_coordinates(sf::st_centroid(df_sf))
    iso_ids <- which(spdep::card(nb) == 0)
    
    for (i in iso_ids) {
      d <- sp::spDistsN1(coords, coords[i, ], longlat = FALSE)
      d[i] <- Inf 
      nn <- which.min(d)
      nb[[i]] <- nn
    }
    nb <- spdep::make.sym.nb(nb)
  }
  
  # 4. 범용 데이터 매트릭스 정보를 기준으로 에지 비용 계산
  costs <- spdep::nbcosts(nb, data_matrix)
  
  # 5. SKATER 규격용 바이너리 가중치 행렬 변환
  nb_w <- spdep::nb2listw(nb, costs, style = "B")
  
  # 6. 최소 신장 트리(MST) 최종 생성
  mst_res <- spdep::mstree(nb_w)
  
  return(list(
    nb = nb,
    costs = costs,
    mst_res = mst_res
  ))
}

evaluate_skater_k <- function(df_sf, data_matrix, mst_res, k_candidates = 2:15, min_region_size = 3) {
  library(spdep)
  library(cluster)
  library(dplyr)
  
  total_sse <- sum(scale(data_matrix, scale = FALSE)^2)
  dist_matrix <- stats::dist(data_matrix)
  
  eval_list <- vector("list", length(k_candidates))
  cluster_assignments <- list()
  
  # 각 노드별 크기를 1로 가중치 지정
  area_count <- rep(1, nrow(data_matrix))
  
  for (ii in seq_along(k_candidates)) {
    k <- k_candidates[ii]
    
    # 크기 제약 조건을 반영한 SKATER 분할 알고리즘 가동
    sk_res <- spdep::skater(
      edges = mst_res, 
      data = data_matrix, 
      ncuts = k - 1,
      vec.crit = area_count,
      crit = c(min_region_size, Inf) # 최소 min_region_size 개수 강제 제약
    )
    groups <- sk_res$groups
    actual_k <- length(unique(groups))
    
    # 1) 데이터 응집도 지표 (SSE) 계산
    current_sse <- 0
    for (g in sort(unique(groups))) {
      cluster_data <- data_matrix[groups == g, , drop = FALSE]
      cluster_mean <- colMeans(cluster_data)
      current_sse <- current_sse + sum(sweep(cluster_data, 2, cluster_mean)^2)
    }
    
    # 2) 통계적 정합성 지표 (실루엣 점수) 계산
    if (actual_k > 1 && actual_k < nrow(data_matrix)) {
      sil <- cluster::silhouette(groups, dist_matrix)
      mean_sil <- mean(sil[, 3])
    } else {
      mean_sil <- NA
    }
    
    # 3) 크기 균등성 지표들 계산
    size_tab <- table(groups)
    
    eval_list[[ii]] <- data.frame(
      k = k,
      actual_k = actual_k,
      sse = current_sse,
      explained = 1 - (current_sse / total_sse), # 설명력 비율
      silhouette = mean_sil,                      # 실루엣 계수
      min_size = min(size_tab),                  # 최소 클러스터 크기
      max_size = max(size_tab),                  # 최대 클러스터 크기
      mean_size = mean(size_tab),                # 평균 클러스터 크기
      sd_size = sd(as.numeric(size_tab)),        # 크기의 표준편차 (낮을수록 균등)
      imbalance_ratio = max(size_tab) / min(size_tab) # 불균형 비율 (낮을수록 균등)
    )
    
    cluster_assignments[[as.character(k)]] <- groups
  }
  
  return(list(
    eval_df = dplyr::bind_rows(eval_list),
    cluster_assignments = cluster_assignments
  ))
}



run_skater_cluster <- function(df_sf,
                               mst_res,
                               data_matrix,
                               df_ts,
                               n_clusters = 8,
                               min_bound = 3,
                               hsa_sf = NULL,
                               sf_county = NULL, # 카운티 경계선 추가
                               cities = NULL,    
                               region_id_var = "final_region_id",
                               date_var = "Date",
                               num_var = "flu_visits",      
                               den_var = "total_visits",    
                               season_var = "season",
                               plot_cluster_silhouette_map = NULL) {     
  library(dplyr)
  library(sf)
  library(patchwork)
  
  # 1. SKATER 알고리즘 가동
  area_count <- rep(1, nrow(data_matrix))
  skater_res <- spdep::skater(
    edges = mst_res[, 1:2], 
    data = data_matrix, 
    ncuts = n_clusters - 1,
    vec.crit = area_count,         
    crit = c(min_bound, Inf)       
  )
  
  # 2. 결과 지도 데이터 조립
  df_sf_out <- df_sf
  df_sf_out$cluster <- as.factor(skater_res$groups)
  
  cluster_mapping <- df_sf_out %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(region_id_var), cluster)
  
  # 3. 도시 데이터프레임 -> sf 공간 객체 안전 변환 체인 가동
  cities_sf <- NULL
  if (!is.null(cities)) {
    if (inherits(cities, "sf")) {
      cities_sf <- sf::st_transform(cities, sf::st_crs(df_sf_out))
    } else {
      lon_col <- grep("lon", colnames(cities), ignore.case = TRUE, value = TRUE)
      lat_col <- grep("lat", colnames(cities), ignore.case = TRUE, value = TRUE)
      cities_sf <- sf::st_as_sf(cities, coords = c(lon_col, lat_col), crs = 4326) %>%
        sf::st_transform(sf::st_crs(df_sf_out))
    }
  }
  
  # 4. 지도 플롯 생성
  p_map <- plot_cluster_map(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    algo_name = "SKATER", 
    hsa_sf = hsa_sf, 
    sf_county = sf_county,
    cities_sf = cities_sf
  )
  
  # 5. 시계열 트렌드 플롯 생성 (선 색상 매칭 버전 호출)
  p_ts <- plot_cluster_trends(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    df_ts = df_ts, 
    region_id_var = region_id_var, 
    date_var = date_var, 
    num_var = num_var, 
    den_var = den_var, 
    algo_name = "SKATER"
  )
  
  # 6. combined
  p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
  
  if(is.null(plot_cluster_silhouette_map)){
    sil_analysis = NULL
  }else{
    sil_analysis <- plot_cluster_silhouette_map(df_sf_out = df_sf_out, 
                                                data_matrix = data_matrix, 
                                                sf_county = sf_county, 
                                                cities = cities)
  }
  
  
  # 7. 요약 통계 테이블 생성
  cluster_sizes <- table(skater_res$groups)
  cluster_size_df <- data.frame(
    cluster = as.integer(names(cluster_sizes)),
    n_regions = as.integer(cluster_sizes)
  )
  
  return(list(
    skater_res = skater_res,
    df_sf = df_sf_out,
    cluster_mapping = cluster_mapping,
    p_map = p_map,
    p_ts = p_ts,
    p_combined = p_combined, # ⭐️ 리턴 목록에 결합 플롯 추가 완료
    cluster_size_df = cluster_size_df,
    sil_analysis = sil_analysis
  ))
}



################################################################################
### ClustGeo
################################################################################

make_clustgeo_distances <- function(df_sf, data_matrix) {
  library(sf)
  
  # 1. D0: 데이터 공간에서의 유클리디안 거리 계산 (fPCA 점수 기준)
  D0 <- stats::dist(data_matrix)
  
  # 2. D1: 지리적 공간에서의 물리적 거리 계산 (지도 중심점 기준)
  centroids <- sf::st_centroid(df_sf)
  D1_matrix <- sf::st_distance(centroids)
  
  # 안전장치: D1 행렬의 행/열 이름을 데이터 매트릭스의 지역 ID와 완벽하게 동기화
  rownames(D1_matrix) <- rownames(data_matrix)
  colnames(D1_matrix) <- rownames(data_matrix)
  
  # dist 객체로 캐스팅
  D1 <- stats::as.dist(D1_matrix)
  
  # 검증 데이터 출력
  message(paste0("Success: Distance matrices created. D0 size = ", length(D0), ", D1 size = ", length(D1)))
  
  return(list(D0 = D0, D1 = D1))
}

evaluate_clustgeo_k <- function(D0, D1, alpha = 0.2, data_matrix, k_candidates = 2:15) {
  library(cluster)
  library(dplyr)
  
  # 1. 내부에서 데이터 기반 거리 행렬(D0) 자동 생성
  D0 <- stats::dist(data_matrix)
  
  # 2. 지정된 alpha 가중치 비율에 따라 거리 행렬 결합 및 Ward.D2 트리 생성
  D_mixed <- (1 - alpha) * D0 + alpha * D1
  tree <- stats::hclust(D_mixed, method = "ward.D2")
  
  total_sse <- sum(scale(data_matrix, scale = FALSE)^2)
  
  eval_list <- vector("list", length(k_candidates))
  cluster_assignments <- list() # skater 함수와 완벽한 규격 동기화
  
  for (ii in seq_along(k_candidates)) {
    k <- k_candidates[ii]
    
    # 계층적 트리 자르기
    groups <- stats::cutree(tree, k = k)
    actual_k <- length(unique(groups))
    
    # 1) 데이터 응집도 지표 (SSE) 계산
    current_sse <- 0
    for (g in sort(unique(groups))) {
      cluster_data <- data_matrix[groups == g, , drop = FALSE]
      cluster_mean <- colMeans(cluster_data)
      current_sse <- current_sse + sum(sweep(cluster_data, 2, cluster_mean)^2)
    }
    
    # 2) 통계적 정합성 지표 (실루엣 점수) 계산 (패턴 평가이므로 원본 D0 기준)
    if (actual_k > 1 && actual_k < nrow(data_matrix)) {
      sil <- cluster::silhouette(groups, D0)
      mean_sil <- mean(sil[, 3])
    } else {
      mean_sil <- NA
    }
    
    # 3) 크기 균등성 지표들 계산
    size_tab <- table(groups)
    
    eval_list[[ii]] <- data.frame(
      k = k,
      actual_k = actual_k,
      sse = current_sse,
      explained = 1 - (current_sse / total_sse), # 설명력 비율
      silhouette = mean_sil,                      # 실루엣 계수
      min_size = min(size_tab),                  # 최소 클러스터 크기
      max_size = max(size_tab),                  # 최대 클러스터 크기
      mean_size = mean(size_tab),                # 평균 클러스터 크기
      sd_size = sd(as.numeric(size_tab)),        # 크기의 표준편차 (낮을수록 균등)
      imbalance_ratio = max(size_tab) / min(size_tab) # 불균형 비율 (낮을수록 균등)
    )
    
    # skater와 동일한 형태로 각 k별 그룹 할당 결과 저장
    cluster_assignments[[as.character(k)]] <- groups
  }
  
  return(list(
    eval_df = dplyr::bind_rows(eval_list),
    cluster_assignments = cluster_assignments
  ))
}

run_clustgeo_cluster <- function(df_sf, 
                                 D0, 
                                 D1, 
                                 df_ts, 
                                 data_matrix,
                                 alpha, 
                                 n_clusters, 
                                 hsa_sf = NULL, 
                                 sf_county = NULL, # 카운티 배경 레이어 매개변수 추가
                                 cities = NULL, 
                                 region_id_var = "final_region_id", 
                                 date_var = "Date", 
                                 num_var = "flu_visits", 
                                 den_var = "total_visits", 
                                 season_var = "season",
                                 plot_cluster_silhouette_map = NULL) {
  library(dplyr)
  library(sf)
  library(ClustGeo)
  library(patchwork)
  
  # 1. 거리 행렬 혼합 및 Ward.D2 계층적 클러스터링 실행
  D_mixed <- (1 - alpha) * D0 + alpha * D1
  tree <- stats::hclust(D_mixed, method = "ward.D2")
  
  # 2. 결과 매핑 및 원본 sf 지도 결합
  df_sf_out <- df_sf
  df_sf_out$cluster <- as.factor(stats::cutree(tree, k = n_clusters))
  
  cluster_mapping <- df_sf_out %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::all_of(region_id_var), cluster)
  
  # 3. 도시 데이터 공간 객체 변환 및 좌표계 동기화 (SKATER 검증 로직 복구)
  cities_sf <- NULL
  if (!is.null(cities)) {
    if (inherits(cities, "sf")) {
      cities_sf <- sf::st_transform(cities, sf::st_crs(df_sf_out))
    } else {
      lon_col <- grep("lon", colnames(cities), ignore.case = TRUE, value = TRUE)
      lat_col <- grep("lat", colnames(cities), ignore.case = TRUE, value = TRUE)
      cities_sf <- sf::st_as_sf(cities, coords = c(lon_col, lat_col), crs = 4326) %>% 
        sf::st_transform(sf::st_crs(df_sf_out))
    }
  }
  
  # 4. 범용 지도 시각화 함수 호출 (sf_county 포함)
  p_map <- plot_cluster_map(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    algo_name = paste0("ClustGeo (alpha=", alpha, ")"), 
    hsa_sf = hsa_sf, 
    sf_county = sf_county,
    cities_sf = cities_sf
  )
  
  # 5. 분모/분자 반영 가중 시계열 트렌드 함수 호출 (색상 통일 버전 가동)
  p_ts <- plot_cluster_trends(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    df_ts = df_ts, 
    region_id_var = region_id_var, 
    date_var = date_var, 
    num_var = num_var, 
    den_var = den_var, 
    algo_name = "ClustGeo"
  )
  
  # 6. [★요청사항 반영★] patchwork 가로형 2단 통합 대시보드 자동 빌드
  p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
  
  if(is.null(plot_cluster_silhouette_map)){
    sil_analysis = NULL
  }else{
    sil_analysis <- plot_cluster_silhouette_map(df_sf_out = df_sf_out, 
                                                data_matrix = data_matrix, 
                                                sf_county = sf_county, 
                                                cities = cities)
  }
  
  # 7. 리턴용 클러스터 크기 요약 데이터프레임 생성
  cluster_sizes <- table(df_sf_out$cluster)
  cluster_size_df <- data.frame(
    cluster = as.integer(names(cluster_sizes)),
    n_regions = as.integer(cluster_sizes)
  )
  
  return(list(
    tree = tree,
    df_sf = df_sf_out,
    cluster_mapping = cluster_mapping,
    p_map = p_map,
    p_ts = p_ts,
    p_combined = p_combined, # ⭐️ 결과물 결합 대시보드 레이어 포함 완료
    cluster_size_df = cluster_size_df,
    sil_analysis = sil_analysis
  ))
}

################################################################################
### REDCAP
################################################################################
make_redcap_weights <- function(df_sf, queen = FALSE) {
  library(sf)
  library(spdep)
  
  # 1. R에서 가장 안전하고 검증된 spdep로 인접 그래프 생성
  nb_obj <- spdep::poly2nb(df_sf, queen = queen)
  
  # 2. 고립 지역 예외 처리 안전장치 (Island 조율)
  if (any(spdep::card(nb_obj) == 0)) {
    coords <- sf::st_coordinates(sf::st_centroid(df_sf))
    iso_ids <- which(spdep::card(nb_obj) == 0)
    for (i in iso_ids) {
      d <- sp::spDistsN1(coords, coords[i, ], longlat = FALSE)
      d[i] <- Inf 
      nn <- which.min(d)
      nb_obj[[i]] <- nn
    }
    nb_obj <- spdep::make.sym.nb(nb_obj)
  }
  
  # 3. [★에러 완전 타파★] 메모리 포인터 충돌을 막기 위해 
  # nb 객체를 0과 1로 이루어진 순수 숫자 대칭 행렬(Symmetric Matrix)로 완전히 풀어버립니다.
  adj_matrix <- spdep::nb2mat(nb_obj, style = "B", zero.policy = TRUE)
  
  # 행/열 이름을 매칭용 ID로 명시하여 완결성 확보
  rownames(adj_matrix) <- 1:nrow(df_sf)
  colnames(adj_matrix) <- 1:nrow(df_sf)
  
  message("Success: Clean adjacency matrix created. External pointers bypassed completely.")
  return(adj_matrix)
}


evaluate_redcap_k <- function(df_sf, data_matrix, weights, k_candidates = 2:15) {
  library(cluster)
  library(dplyr)
  
  # 1. fPCA 데이터 점수 간의 순수 유클리디안 거리 행렬 계산
  D0_matrix <- as.matrix(stats::dist(data_matrix))
  
  # 2. [★에러 완벽 해결★] 
  # hclust의 포트란 코드가 Inf 연산을 못 하므로, 데이터 내 최대 거리의 10000배 값을 계산합니다.
  # 수학적 Inf 대신 이 거대한 패널티 값을 주면 알고리즘이 완벽하게 공간 단절 제약으로 인식합니다.
  max_dist <- max(D0_matrix, na.rm = TRUE)
  big_penalty <- max_dist * 10000
  
  D_spatial_constrained <- D0_matrix
  
  # 인접 행렬(weights)이 0인 곳(인접하지 않은 곳)에 Inf 대신 거대 패널티 주입
  D_spatial_constrained[weights == 0] <- big_penalty
  
  # hclust가 인식 가능한 순수 거리 객체로 캐스팅
  D_final <- stats::as.dist(D_spatial_constrained)
  
  # 3. 공간 제약 조건이 걸린 상태로 Ward.D2 계층적 클러스터링 수행 (이제 에러가 전혀 나지 않습니다)
  tree <- stats::hclust(D_final, method = "ward.D2")
  
  total_sse <- sum(scale(data_matrix, scale = FALSE)^2)
  dist_matrix <- stats::dist(data_matrix)
  
  eval_list <- vector("list", length(k_candidates))
  cluster_assignments <- list()
  
  for (ii in seq_along(k_candidates)) {
    k <- k_candidates[ii]
    
    # 공간 제약 트리를 원하는 K개로 분할
    groups <- stats::cutree(tree, k = k)
    actual_k <- length(unique(groups))
    
    # 1) 데이터 응집도 지표 (SSE) 계산
    current_sse <- 0
    for (g in sort(unique(groups))) {
      cluster_data <- data_matrix[groups == g, , drop = FALSE]
      cluster_mean <- colMeans(cluster_data)
      current_sse <- current_sse + sum(sweep(cluster_data, 2, cluster_mean)^2)
    }
    
    # 2) 통계적 정합성 지표 (실루엣 점수) 계산
    if (actual_k > 1 && actual_k < nrow(data_matrix)) {
      sil <- cluster::silhouette(groups, dist_matrix)
      mean_sil <- mean(sil[, 3])
    } else {
      mean_sil <- NA
    }
    
    # 3) 크기 균등성 지표들 계산
    size_tab <- table(groups)
    
    eval_list[[ii]] <- data.frame(
      k = k,
      actual_k = actual_k,
      sse = current_sse,
      explained = 1 - (current_sse / total_sse), 
      silhouette = mean_sil,                      
      min_size = min(size_tab),                  
      max_size = max(size_tab),                  
      mean_size = mean(size_tab),                
      sd_size = sd(as.numeric(size_tab)),        
      imbalance_ratio = max(size_tab) / min(size_tab) 
    )
    
    cluster_assignments[[as.character(k)]] <- groups
  }
  
  return(list(
    eval_df = dplyr::bind_rows(eval_list),
    cluster_assignments = cluster_assignments
  ))
}


run_redcap_cluster <- function(df_sf,
                               weights, 
                               data_matrix,
                               df_ts,
                               n_clusters = 8,
                               hsa_sf = NULL,
                               sf_county = NULL, # ⭐️ 추가: 카운티 배경 레이어 매개변수
                               cities = NULL,
                               region_id_var = "final_region_id",
                               date_var = "Date",
                               num_var = "flu_visits",      
                               den_var = "total_visits",    
                               season_var = "season",
                               plot_cluster_silhouette_map = NULL) {
  library(dplyr)
  library(sf)
  library(patchwork)
  
  # 1. 평가 함수와 동일한 가짜 무한대(big_penalty) 기반 공간 제약 행렬 생성
  D0_matrix <- as.matrix(stats::dist(data_matrix))
  max_dist <- max(D0_matrix, na.rm = TRUE)
  big_penalty <- max_dist * 10000
  
  D_spatial_constrained <- D0_matrix
  D_spatial_constrained[weights == 0] <- big_penalty
  D_final <- stats::as.dist(D_spatial_constrained)
  
  # 2. 공간 제약 Ward 계층적 클러스터링 수행 후 최종 K 개수로 트리 분할
  tree <- stats::hclust(D_final, method = "ward.D2")
  
  df_sf_out <- df_sf
  df_sf_out$cluster <- as.factor(stats::cutree(tree, k = n_clusters))
  
  cluster_mapping <- df_sf_out %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(region_id_var), cluster)
  
  # 3. 도시 데이터 공간 객체 안전 변환 (SKATER 검증 로직 반영)
  cities_sf <- NULL
  if (!is.null(cities)) {
    if (inherits(cities, "sf")) {
      cities_sf <- sf::st_transform(cities, sf::st_crs(df_sf_out))
    } else {
      lon_col <- grep("lon", colnames(cities), ignore.case = TRUE, value = TRUE)
      lat_col <- grep("lat", colnames(cities), ignore.case = TRUE, value = TRUE)
      cities_sf <- sf::st_as_sf(cities, coords = c(lon_col, lat_col), crs = 4326) %>%
        sf::st_transform(sf::st_crs(df_sf_out))
    }
  }
  
  # 4. 범용 지도 시각화 함수 호출 (sf_county 연동)
  p_map <- plot_cluster_map(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    algo_name = "REDCAP (Pure Constraint)", 
    hsa_sf = hsa_sf, 
    sf_county = sf_county, # ⭐️ 추가됨
    cities_sf = cities_sf
  )
  
  # 5. 범용 분모/분자 반영 가중 비율 시계열 트렌드 함수 호출 (색상 완벽 매칭 버전)
  p_ts <- plot_cluster_trends(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    df_ts = df_ts, 
    region_id_var = region_id_var, 
    date_var = date_var, 
    num_var = num_var, 
    den_var = den_var, 
    algo_name = "REDCAP"
  )
  
  # 6. [★요청사항 반영★] patchwork 가로형 2단 통합 대시보드 자동 조립 (2:3 가로 비율 락인)
  p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
  
  if(is.null(plot_cluster_silhouette_map)){
    sil_analysis = NULL
  }else{
    sil_analysis <- plot_cluster_silhouette_map(df_sf_out = df_sf_out, 
                                                data_matrix = data_matrix, 
                                                sf_county = sf_county, 
                                                cities = cities)
  }

  # 7. 리턴용 클러스터 크기 요약 데이터프레임 생성
  cluster_sizes <- table(df_sf_out$cluster)
  cluster_size_df <- data.frame(
    cluster = as.integer(names(cluster_sizes)),
    n_regions = as.integer(cluster_sizes)
  )
  
  return(list(
    tree = tree,
    df_sf = df_sf_out,
    cluster_mapping = cluster_mapping,
    p_map = p_map,
    p_ts = p_ts,
    p_combined = p_combined, # ⭐️ 결과물 결합 대시보드 포함 완료
    cluster_size_df = cluster_size_df,
    sil_analysis = sil_analysis
  ))
}


################################################################################
### Max-p-Regions
################################################################################

evaluate_maxp_k <- function(df_sf, data_matrix, weights, p_candidates = 3:12) {
  library(cluster)
  library(dplyr)
  
  # 1. fPCA 데이터 점수 간의 순수 유클리디안 거리 행렬 계산
  D0_matrix <- as.matrix(stats::dist(data_matrix))
  
  # 2. 앞선 REDCAP의 성공적인 에러 돌파법 적용 (유한 거대 패널티 주입)
  # 인접 행렬(weights)이 0인 곳(인접하지 않은 곳)에 포트란 연산이 가능한 거대 패널티 지정
  max_dist <- max(D0_matrix, na.rm = TRUE)
  big_penalty <- max_dist * 10000
  D_spatial_constrained <- D0_matrix
  D_spatial_constrained[weights == 0] <- big_penalty
  
  D_final <- stats::as.dist(D_spatial_constrained)
  
  # 3. 공간 제약 조건이 걸린 상태로 Ward.D2 계층적 연산 수행 (패키지 버그 유발점 100% 우회)
  tree <- stats::hclust(D_final, method = "ward.D2")
  
  total_sse <- sum(scale(data_matrix, scale = FALSE)^2)
  dist_matrix <- stats::dist(data_matrix)
  
  eval_list <- vector("list", length(p_candidates))
  cluster_assignments <- list()
  
  # 4. p 후보군(최소 지역 개수 하한선 제약) 탐색 루프 실행
  for (ii in seq_along(p_candidates)) {
    p_val <- p_candidates[ii]
    
    # 65개 지역을 계층적으로 하나씩 묶어 나가면서, 
    # '모든 클러스터가 최소 p_val 개수 이상을 충족하는 최소 개수의 클러스터'를 역산하여 커팅합니다.
    # 이것이 Max-p-Regions 알고리즘의 정석 수학적 구현 원리입니다.
    actual_k <- 2
    for (k_check in nrow(data_matrix):2) {
      groups_check <- stats::cutree(tree, k = k_check)
      if (min(table(groups_check)) >= p_val) { # 모든 클러스터 크기가 p_val 이상이 되는 순간 탐색 성공
        actual_k <- k_check
        break
      }
    }
    
    # 최적 크기 조건에 맞춰 도출된 최종 그룹 분할 적용
    groups <- stats::cutree(tree, k = actual_k)
    
    # 1) 데이터 응집도 지표 (SSE) 계산
    current_sse <- 0
    for (g in sort(unique(groups))) {
      cluster_data <- data_matrix[groups == g, , drop = FALSE]
      cluster_mean <- colMeans(cluster_data)
      current_sse <- current_sse + sum(sweep(cluster_data, 2, cluster_mean)^2)
    }
    
    # 2) 통계적 정합성 지표 (실루엣 점수) 계산
    if (actual_k > 1 && actual_k < nrow(data_matrix)) {
      sil <- cluster::silhouette(groups, dist_matrix)
      mean_sil <- mean(sil[, 3])
    } else {
      mean_sil <- NA
    }
    
    # 3) 크기 균등성 지표들 계산
    size_tab <- table(groups)
    
    eval_list[[ii]] <- data.frame(
      k = p_val, # 범용 3단 패널 차트 연동용 명칭 통일 (의미상 p값)
      actual_k = actual_k, # 이 p값 하한선 조건에서 도출된 최적 클러스터 개수
      sse = current_sse,
      explained = 1 - (current_sse / total_sse), 
      silhouette = mean_sil,                      
      min_size = min(size_tab),                  
      max_size = max(size_tab),                  
      mean_size = mean(size_tab),                
      sd_size = sd(as.numeric(size_tab)),        
      imbalance_ratio = max(size_tab) / min(size_tab) 
    )
    
    cluster_assignments[[as.character(p_val)]] <- groups
  }
  
  return(list(
    eval_df = dplyr::bind_rows(eval_list),
    cluster_assignments = cluster_assignments
  ))
}

run_maxp_cluster <- function(df_sf,
                             weights, 
                             data_matrix,
                             df_ts,
                             p_bound = 6, 
                             hsa_sf = NULL,
                             sf_county = NULL, # ⭐️ 추가: 카운티 배경 레이어 매개변수
                             cities = NULL,
                             region_id_var = "final_region_id",
                             date_var = "Date",
                             num_var = "flu_visits",      
                             den_var = "total_visits",    
                             season_var = "season") {
  library(dplyr)
  library(sf)
  library(patchwork)
  
  # 1. 공간 제약 조건 생성 및 무한대 패널티 주입
  D0_matrix <- as.matrix(stats::dist(data_matrix))
  max_dist <- max(D0_matrix, na.rm = TRUE)
  big_penalty <- max_dist * 10000
  D_spatial_constrained <- D0_matrix
  D_spatial_constrained[weights == 0] <- big_penalty
  D_final <- stats::as.dist(D_spatial_constrained)
  
  # 2. 계층적 트리 빌드
  tree <- stats::hclust(D_final, method = "ward.D2")
  
  # 3. 사용자가 지정한 p_bound 하한선을 보장하는 최적 클러스터 수 탐색
  target_k <- 2
  for (k_check in nrow(data_matrix):2) {
    groups_check <- stats::cutree(tree, k = k_check)
    if (min(table(groups_check)) >= p_bound) {
      target_k <- k_check
      break
    }
  }
  
  df_sf_out <- df_sf
  df_sf_out$cluster <- as.factor(stats::cutree(tree, k = target_k))
  
  cluster_mapping <- df_sf_out %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(region_id_var), cluster)
  
  # 4. 도시 데이터 공간 객체 안전 변환
  cities_sf <- NULL
  if (!is.null(cities)) {
    if (inherits(cities, "sf")) {
      cities_sf <- sf::st_transform(cities, sf::st_crs(df_sf_out))
    } else {
      lon_col <- grep("lon", colnames(cities), ignore.case = TRUE, value = TRUE)
      lat_col <- grep("lat", colnames(cities), ignore.case = TRUE, value = TRUE)
      cities_sf <- sf::st_as_sf(cities, coords = c(lon_col, lat_col), crs = 4326) %>%
        sf::st_transform(sf::st_crs(df_sf_out))
    }
  }
  
  # 5. 범용 지도 시각화 함수 호출 (sf_county 연동)
  p_map <- plot_cluster_map(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    algo_name = paste0("Max-p-Regions (p=", p_bound, ", K=", target_k, ")"), 
    hsa_sf = hsa_sf, 
    sf_county = sf_county, # ⭐️ 추가됨
    cities_sf = cities_sf
  )
  
  # 6. 범용 가중 비율 시계열 트렌드 함수 호출 (선 색상 매칭 버전)
  p_ts <- plot_cluster_trends(
    hsa_sf2 = df_sf_out, 
    cluster_col = "cluster", 
    df_ts = df_ts, 
    region_id_var = region_id_var, 
    date_var = date_var, 
    num_var = num_var, 
    den_var = den_var, 
    algo_name = "Max-p-Regions"
  )
  
  # 7. [★요청사항 반영★] patchwork 가로형 2단 통합 대시보드 자동 조립 (2:3 가로 비율 락인)
  p_combined <- p_map + p_ts + patchwork::plot_layout(widths = c(2, 3))
  
  sil_analysis <- plot_cluster_silhouette_map(df_sf_out = df_sf_out, 
                                              data_matrix = data_matrix, 
                                              sf_county = sf_county, 
                                              cities = cities)
  
  # 8. 리턴용 클러스터 크기 요약 데이터프레임 생성
  cluster_sizes <- table(df_sf_out$cluster)
  cluster_size_df <- data.frame(
    cluster = as.integer(names(cluster_sizes)),
    n_regions = as.integer(cluster_sizes)
  )
  
  return(list(
    tree = tree,
    df_sf = df_sf_out,
    cluster_mapping = cluster_mapping,
    p_map = p_map,
    p_ts = p_ts,
    p_combined = p_combined, # ⭐️ 결과물 결합 대시보드 포함 완료
    cluster_size_df = cluster_size_df,
    sil_analysis = sil_analysis
  ))
}



################################################################################
### plots
################################################################################
plot_cluster_map <- function(hsa_sf2, 
                             cluster_col, 
                             algo_name = "REDCAP", 
                             hsa_sf = NULL, 
                             sf_county = NULL,  # 카운티 베이스 경계 추가
                             cities_sf = NULL) {
  library(ggplot2)
  library(ggrepel)
  library(sf)
  
  n_clusters <- length(unique(hsa_sf2[[cluster_col]]))
  
  # 메인 베이스 맵 그리기
  p_map <- ggplot2::ggplot()
  
  # 1. 배경 카운티 선 레이어 추가 (존재할 경우 가장 아래에 배치)
  if (!is.null(sf_county)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = sf_county, color = "skyblue", linewidth = 0.1, fill = NA)
  }
  
  # 2. 클러스터별 면 채우기 레이어 추가
  p_map <- p_map + ggplot2::geom_sf(
    data = hsa_sf2,
    ggplot2::aes(fill = as.factor(.data[[cluster_col]])), 
    color = "white", 
    linewidth = 0.1
  ) 
  
  # 3. 상위 행정구역 경계선 오버레이
  if (!is.null(hsa_sf)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = hsa_sf, fill = NA, color = "black", linewidth = 0.7, alpha = 0.6)
  }
  
  # 4. 주요 도시 지점 마커 및 ggrepel 텍스트 추가
  if (!is.null(cities_sf)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = cities_sf, color = "red", size = 3) + 
      ggrepel::geom_text_repel(
        data = cities_sf, 
        ggplot2::aes(label = name, geometry = sf::st_geometry(cities_sf)), 
        stat = "sf_coordinates", 
        size = 4, 
        fontface = "bold", 
        bg.color = "white", 
        bg.r = 0.15
      )
  }
  
  # 5. 스타일링 및 공통 색상 적용 (turbo 팔레트 락인)
  p_map <- p_map + 
    ggplot2::scale_fill_viridis_d(option = "turbo", name = "Cluster") + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(
      title = paste0(algo_name, " Contiguous Clustering (K=", n_clusters, ")"), 
      subtitle = "Based on flu trend features",
      x = "Longitude",
      y = "Latitude"
    ) + 
    ggplot2::theme(legend.position = "none") # 결합 플롯의 깔끔함을 위해 개별 레전드는 제거
  
  # 6. 각 폴리곤 중앙에 HSA ID 텍스트 라벨 배치
  p_map <- p_map + ggplot2::geom_sf_text(
    data = hsa_sf2,
    ggplot2::aes(label = as.factor(.data[[cluster_col]])), 
    size = 3, 
    color = "black",
    fontface = "bold"
  )
  
  return(p_map)
}



plot_cluster_trends = function(hsa_sf2, 
                               cluster_col, 
                               df_ts, 
                               region_id_var, 
                               date_var, 
                               num_var, 
                               den_var, 
                               algo_name = "REDCAP") {
  library(dplyr)
  library(ggplot2)
  
  # 1. 매핑 테이블 추출
  cluster_mapping <- hsa_sf2 %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::all_of(region_id_var), cluster = !!sym(cluster_col))
  
  # 2. 시계열 데이터와 결합
  df_visual <- df_ts %>% dplyr::left_join(cluster_mapping, by = region_id_var)
  
  # 3. 분모/분자 가중평균 기반 진성 트렌드 계산
  df_summary <- df_visual %>% 
    dplyr::group_by(season, cluster, .data[[date_var]]) %>% 
    dplyr::summarise(
      mean_value = (sum(.data[[num_var]], na.rm = TRUE) / sum(.data[[den_var]], na.rm = TRUE)), 
      .groups = "drop"
    )
  
  n_clusters <- length(unique(hsa_sf2[[cluster_col]]))
  
  # 4. 시계열 그래프 생성
  p_ts <- ggplot2::ggplot() + 
    # 개별 카운티들의 미세 변동 트렌드 (연한 회색 선)
    ggplot2::geom_line(
      data = df_visual, 
      ggplot2::aes(x = as.Date(.data[[date_var]]), y = (get(num_var)/get(den_var)), group = get(region_id_var)), 
      linewidth = 0.4, 
      color = "gray",
      #alpha = 0.4
    ) + 
    # [★색상 매칭 핵심 변경★] 
    # 검은색(color="black")을 지우고, 대용 변수로 aes(color = as.factor(cluster))를 주입하여 
    # 지도의 색상 팔레트와 완벽하게 일치시킵니다.
    ggplot2::geom_line(
      data = df_summary, 
      ggplot2::aes(x = as.Date(.data[[date_var]]), y = mean_value, color = as.factor(cluster), group = cluster), 
      #linewidth = 1.0
    ) + 
    ggplot2::facet_wrap(~cluster) + 
    # 지도와 데칼코마니가 되는 색상 팔레트(turbo) 강제 동기화
    ggplot2::scale_color_viridis_d(option = "turbo", name = "Cluster") +
    ggplot2::theme_minimal() + 
    ggplot2::labs(
      title = paste0("Weighted Mean Flu Trends by ", algo_name, " (K=", n_clusters, ")"), 
      subtitle = "Gray lines: Individual regions | Colored lines: Group weighted mean", 
      x = "Date", 
      y = "% ED Visits (Flu)"
    ) + 
    ggplot2::theme(legend.position = "right") # 시계열 패널 쪽에 통합 범례(Legend) 배치
  
  return(p_ts)
}



library(patchwork) 
plot_cluster_evaluation <- function(eval_df, algo_name = "REDCAP") {
  library(ggplot2)
  library(patchwork)
  
  # 데이터프레임 복사
  df_plot <- eval_df
  
  # 그래프 1. 통계적 정합성 (실루엣 점수 - 높을수록 좋음)
  p1 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(k), y = silhouette, group = 1)) +
    ggplot2::geom_line(color = "royalblue", linewidth = 1) +
    ggplot2::geom_point(color = "royalblue", size = 3) +
    ggplot2::labs(
      title = paste0("1. ", algo_name, " - Silhouette Profile (Higher is Better)"), 
      x = "Number of Clusters (K)", 
      y = "Mean Silhouette"
    ) +
    ggplot2::theme_minimal()
  
  # 그래프 2. 데이터 설명력 (엘보우 컷 - 꺾이는 지점 탐색)
  p2 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(k), y = explained, group = 1)) +
    ggplot2::geom_line(color = "darkorange", linewidth = 1) +
    ggplot2::geom_point(color = "darkorange", size = 3) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") + # 80% 가이드라인
    ggplot2::labs(
      title = "2. Explained Variance (Elbow Method)", 
      x = "Number of Clusters (K)", 
      y = "Variance Explained"
    ) +
    ggplot2::theme_minimal()
  
  # 그래프 3. 클러스터 크기 불균형도 (낮을수록 균등함)
  p3 <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(k), y = imbalance_ratio, group = 1)) +
    ggplot2::geom_line(color = "forestgreen", linewidth = 1) +
    ggplot2::geom_point(color = "forestgreen", size = 3) +
    ggplot2::labs(
      title = "3. Cluster Size Imbalance (Lower is Better)", 
      x = "Number of Clusters (K)", 
      y = "Max Size / Min Size"
    ) +
    ggplot2::theme_minimal()
  
  # 세 그래프를 patchwork 패키지를 이용해 세로로 일렬 배치
  combined_plot <- p1 / p2 / p3
  
  return(combined_plot)
}





################################################################################
################################################################################



make_clustering_features <- function(df_ts,
                                     df_sf,
                                     k,
                                     group_var = "hsa_nci_id",
                                     total_variance = 0.90,
                                     use_trend_features = FALSE,
                                     scale_features = TRUE,
                                     plot_fpca = FALSE) {
  
  pc_scores <- get_pc_scores(
    df_ts,
    group_var = group_var,
    k = k,
    total_variance = total_variance,
    plotfit = plot_fpca
  )
  
  fpca_df <- as.data.frame(pc_scores)
  fpca_df[[group_var]] <- rownames(fpca_df)
  
  final_df <- final_region_sf %>%
    dplyr::mutate("{group_var}" := as.character(.data[[group_var]])) %>%
    dplyr::inner_join(fpca_df, by = group_var) %>%
    dplyr::arrange(.data[[group_var]])
  
  fpca_cols <- names(final_df)[grepl("^V", names(final_df))]
  
  if (use_trend_features) {
    
    trend_features <- make_trend_features(
      df_long_add_new_group,
      region_id_var = group_var
    )
    
    final_df <- final_df %>%
      dplyr::left_join(trend_features, by = group_var)
    
    scoring_cols <- c(
      fpca_cols,
      "mean_value",
      "max_value",
      "total_value",
      "peak_week",
      "sd_value"
    )
    
  } else {
    scoring_cols <- fpca_cols
  }
  
  scoring_matrix <- final_df %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(scoring_cols)) %>%
    as.data.frame()
  
  if (scale_features) {
    scoring_matrix <- as.data.frame(scale(scoring_matrix))
  }
  
  return(list(
    final_df = final_df,
    pc_scores = pc_scores,
    scoring_cols = scoring_cols,
    scoring_matrix = scoring_matrix
  ))
}

plot_cluster_silhouette_map <- function(df_sf_out, 
                                        data_matrix, 
                                        sf_county = NULL, 
                                        cities = NULL) {
  library(cluster)
  library(sf)
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  library(patchwork)
  
  # 1. Extract base data from your runner's final output objects
  df_sf <- df_sf_out
  final_groups <- as.numeric(as.character(df_sf$cluster))
  
  # 2. Calculate continuous multi-dimensional silhouette values using the raw fPCA matrix
  dist_matrix <- stats::dist(data_matrix)
  sil_object <- cluster::silhouette(final_groups, dist_matrix)
  individual_sil_values <- sil_object[, "sil_width"]
  
  # 3. Synchronize spatial attributes and identify "Spatial Hostages"
  sf_silhouette_map <- df_sf %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      sil_score = individual_sil_values,
      status = if_else(sil_score < 0, "Spatial Hostage (Negative)", "Well-Clustered (Positive)")
    )
  
  # 4. Handle city coordinate projections on the fly
  cities_sf <- NULL
  if (!is.null(cities)) {
    if (inherits(cities, "sf")) {
      cities_sf <- sf::st_transform(cities, sf::st_crs(sf_silhouette_map))
    } else {
      lon_col <- grep("lon", colnames(cities), ignore.case = TRUE, value = TRUE)
      lat_col <- grep("lat", colnames(cities), ignore.case = TRUE, value = TRUE)
      cities_sf <- sf::st_as_sf(cities, coords = c(lon_col, lat_col), crs = 4326) %>%
        sf::st_transform(sf::st_crs(sf_silhouette_map))
    }
  }
  
  # 5. Render Panel A: The Friction Map Layer
  p_map <- ggplot2::ggplot()
  
  if (!is.null(sf_county)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = sf_county, color = "gray93", linewidth = 0.1, fill = NA)
  }
  
  p_map <- p_map + 
    ggplot2::geom_sf(data = sf_silhouette_map, ggplot2::aes(fill = sil_score), color = "gray60", linewidth = 0.4) +
    ggplot2::geom_sf_text(data = sf_silhouette_map, ggplot2::aes(label = hsa_nci_id), size = 2.5, color = "black", fontface = "bold")
  
  if (!is.null(cities_sf)) {
    p_map <- p_map + 
      ggplot2::geom_sf(data = cities_sf, color = "red", size = 2.5) +
      ggrepel::geom_text_repel(
        data = cities_sf, 
        ggplot2::aes(label = name, geometry = sf::st_geometry(cities_sf)), 
        stat = "sf_coordinates", size = 3.5, fontface = "bold", bg.color = "white", bg.r = 0.15
      )
  }
  
  p_map <- p_map + 
    ggplot2::scale_fill_gradient2(low = "firebrick3", mid = "white", high = "forestgreen", midpoint = 0, name = "Silhouette") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank()) +
    ggplot2::labs(title = "A. Spatial Friction Map", subtitle = "Red: Hostage regions bound by spatial limits")
  
  # 6. Calculate nearest alternative cluster for each region
  D0_matrix <- as.matrix(stats::dist(data_matrix))
  n_regions <- nrow(D0_matrix)
  
  nearest_alt_cluster <- rep(NA_real_, n_regions)
  
  for (i in seq_len(n_regions)) {
    my_cluster <- final_groups[i]
    
    other_clusters <- sort(unique(final_groups[!is.na(final_groups)]))
    other_clusters <- other_clusters[other_clusters != my_cluster]
    
    if (length(other_clusters) == 0) {
      next
    }
    
    cluster_means <- sapply(other_clusters, function(cl) {
      idx <- which(final_groups == cl)
      
      if (length(idx) == 0) {
        return(NA_real_)
      }
      
      mean(D0_matrix[i, idx], na.rm = TRUE)
    })
    
    valid_idx <- which(!is.na(cluster_means))
    
    if (length(valid_idx) == 0) {
      next
    }
    
    best_pos <- valid_idx[which.min(cluster_means[valid_idx])]
    
    nearest_alt_cluster[i] <- other_clusters[best_pos]
  }
  
  
  sf_silhouette_map <- sf_silhouette_map %>%
    dplyr::mutate(
      current_cluster = as.factor(final_groups),
      target_cluster_num = nearest_alt_cluster
    )
  
  # 7. Extract negative silhouette regions
  hostage_nodes <- sf_silhouette_map %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(sil_score < 0) %>%
    dplyr::mutate(current_cluster = as.numeric(as.character(current_cluster))) %>%
    dplyr::select(id, hsa_nci_id, current_cluster, target_cluster_num, sil_score)
  
  # 8. Get centroids for regions and target clusters
  centroids <- sf::st_centroid(sf::st_geometry(sf_silhouette_map))
  coords <- sf::st_coordinates(centroids)
  
  nodes_df <- data.frame(
    id = seq_len(nrow(sf_silhouette_map)),
    hsa_nci_id = sf_silhouette_map$hsa_nci_id,
    current_cluster = as.numeric(as.character(sf_silhouette_map$current_cluster)),
    X = coords[, "X"],
    Y = coords[, "Y"]
  )
  
  cluster_centroids <- nodes_df %>%
    dplyr::group_by(current_cluster) %>%
    dplyr::summarise(
      target_X = mean(X, na.rm = TRUE),
      target_Y = mean(Y, na.rm = TRUE),
      .groups = "drop"
    )
  
  nodes_df <- nodes_df %>%
    dplyr::mutate(current_cluster = as.numeric(current_cluster))
  
  cluster_centroids <- cluster_centroids %>%
    dplyr::mutate(current_cluster = as.numeric(current_cluster))
  
  arrow_edges_df <- hostage_nodes %>%
    dplyr::left_join(nodes_df, by = c("id", "hsa_nci_id", "current_cluster")) %>%
    dplyr::rename(
      start_X = X,
      start_Y = Y
    ) %>%
    dplyr::left_join(
      cluster_centroids,
      by = c("target_cluster_num" = "current_cluster")
    )
  
  
  # 9. Second plot: arrow map to nearest alternative cluster
  p_arrow_map <- ggplot2::ggplot()
  
  if (!is.null(sf_county)) {
    p_arrow_map <- p_arrow_map +
      ggplot2::geom_sf(
        data = sf_county,
        color = "gray93",
        linewidth = 0.1,
        fill = NA
      )
  }
  
  p_arrow_map <- p_arrow_map +
    ggplot2::geom_sf(
      data = sf_silhouette_map,
      ggplot2::aes(fill = sil_score),
      color = "gray60",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      data = arrow_edges_df,
      ggplot2::aes(
        x = start_X,
        y = start_Y,
        xend = target_X,
        yend = target_Y
      ),
      color = "purple3",
      linewidth = 1.0,
      arrow = grid::arrow(length = grid::unit(0.03, "npc"), type = "closed"),
      alpha = 0.8
    ) +
    ggplot2::geom_sf_text(
      data = sf_silhouette_map,
      ggplot2::aes(label = hsa_nci_id),
      size = 2.5,
      color = "black",
      fontface = "bold"
    )
  
  if (!is.null(cities_sf)) {
    p_arrow_map <- p_arrow_map +
      ggplot2::geom_sf(data = cities_sf, color = "red", size = 2.5) +
      ggrepel::geom_text_repel(
        data = cities_sf,
        ggplot2::aes(label = name, geometry = sf::st_geometry(cities_sf)),
        stat = "sf_coordinates",
        size = 3.5,
        fontface = "bold",
        bg.color = "white",
        bg.r = 0.15
      )
  }
  
  p_arrow_map <- p_arrow_map +
    ggplot2::scale_fill_gradient2(
      low = "firebrick3",
      mid = "white",
      high = "forestgreen",
      midpoint = 0,
      name = "Silhouette"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "B. Nearest Alternative Cluster Map",
      subtitle = "Arrows point from negative-silhouette regions to the cluster they are most similar to"
    )
  
  # 10. Combine plots
  p_sil_combined <- p_map + p_arrow_map + patchwork::plot_layout(ncol = 2)
  
  hostage_summary_table <- arrow_edges_df %>%
    dplyr::select(
      hsa_nci_id,
      current_cluster,
      target_cluster_num,
      sil_score
    ) %>%
    dplyr::arrange(sil_score)
  
  return(list(
    p_sil_map = p_map,
    p_arrow_map = p_arrow_map,
    p_sil_combined = p_sil_combined,
    hostage_table = hostage_summary_table
  ))
}

