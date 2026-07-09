# Augmented Spatial Clustering for Texas Influenza Dynamics

This page documents the county-level clustering methods used for influenza ED
visit dynamics, with emphasis on the augmented ClustGeo and augmented REDCAP
runs.

The goal is to compare data-driven geographic groupings against existing Texas
boundaries:

- State
- DSHS regions
- RAC regions
- HSA regions
- County

The candidate cluster counts used here are:

```text
K = 7, 9, 15, 21, 23, 31, 45, 61
```

The clustering was built with leave-one-season-out training for:

```text
2023/24, 2024/25, 2025/26
```

For example, the clusters used to evaluate the `2024/25` season were created
without using `2024/25` data.

---

## Output Locations

Cluster assignment files:

[../data/cluster_data_season](../data/cluster_data_season)

Cluster map and trend figures:

[../figures/cluster_combine](../figures/cluster_combine)

Spatial variation results:

- [../results/spatial_variation_clustergeoaug.csv](../results/spatial_variation_clustergeoaug.csv)
- [../results/spatial_variation_redcapaug.csv](../results/spatial_variation_redcapaug.csv)

Spatial variation figures:

- [../figures/summary/spatial_variation_clustergeoaug.png](../figures/summary/spatial_variation_clustergeoaug.png)
- [../figures/summary/spatial_variation_redcapaug.png](../figures/summary/spatial_variation_redcapaug.png)

---

## Data and Training Window

The base input is county-level weekly flu ED visit data:

```text
data/county_edvisits.csv
```

Main variables:

| Variable | Meaning |
|---|---|
| `season` | Influenza season |
| `Date` | Weekly date |
| `county` | Texas county |
| `hsa_nci_id` | HSA identifier |
| `value_flu` | Flu ED visits |
| `value_all` | All ED visits |
| `value` | Flu ED visit proportion, `value_flu / value_all` |
| `population` | County population |

Clustering features are computed only from flu-season months:

```text
October, November, December, January, February, March
```

This focuses the clustering on the period where influenza dynamics are most
relevant for forecasting.

---

## Augmented Feature Matrix

The original clustering used FPCA scores from smoothed flu time series. That is
useful, but FPCA alone may miss interpretable epidemic features such as onset
timing, peak timing, and rise/decline behavior. The augmented runs keep FPCA and
add seasonal flu features.

For county `i`, week `t`, and season `s`, let `F` be flu ED visits and `A` be
all ED visits. The observed weekly flu ED visit proportion is:

$$
y_{i,s,t} = \frac{F_{i,s,t}}{A_{i,s,t}}
$$

A three-week centered rolling mean is applied within each season:

$$
\tilde{y}_{i,s,t}
= \frac{y_{i,s,t-1} + y_{i,s,t} + y_{i,s,t+1}}{3}
$$

Smoothing is done within each season only. This avoids artificial smoothing from
the end of one season into the beginning of the next.

### FPCA Features

Each county's smoothed flu curve is represented as a functional observation:

$$
\tilde{y}_i(t)
$$

FPCA approximates each county curve as:

$$
\tilde{y}_i(t)
= \mu(t) + \sum_{m=1}^{M} \xi_{i,m}\phi_m(t) + \varepsilon_i(t)
$$

where:

- $\mu(t)$ is the mean flu curve
- $\phi_m(t)$ is FPCA component $m$
- $\xi_{i,m}$ is the FPCA score for county $i$ on component $m$

The FPCA scores form the first part of the clustering feature vector.

### Seasonal Epidemiologic Features

For each county-season, the following features are calculated from the smoothed
curve:

| Feature | Definition |
|---|---|
| Mean incidence | Average smoothed flu ED visit proportion |
| Peak incidence | Maximum smoothed flu ED visit proportion |
| Seasonal burden | Sum of smoothed flu ED visit proportions over the season |
| Peak week | Week of maximum smoothed incidence |
| Onset week | First week reaching 20% of that county-season peak |
| Duration above threshold | Number of weeks above 20% of peak |
| Growth slope | Slope from first week to peak week |
| Decline slope | Slope from peak week to last week |
| Mean denominator | Average total ED visits, included as a reliability/volume signal |

For example, peak incidence is:

$$
p_{i,s} = \max_t \tilde{y}_{i,s,t}
$$

Onset week is:

$$
o_{i,s} = \min \{ t : \tilde{y}_{i,s,t} \ge 0.2 p_{i,s} \}
$$

Growth slope is:

$$
g_{i,s} = \frac{p_{i,s} - \tilde{y}_{i,s,1}}{t^*_{i,s} - 1}
$$

Decline slope is:

$$
d_{i,s} = \frac{\tilde{y}_{i,s,T} - p_{i,s}}{T - t^*_{i,s}}
$$

Here, `p` is peak incidence, `o` is onset week, `g` is growth slope, `d` is
decline slope, and `t*` is the peak week.

The seasonal features are then averaged across training seasons. Season-to-season
standard deviations are also included, so counties with unstable seasonal
patterns can be separated from counties with stable patterns.

### Final Feature Vector

The final augmented feature vector for county `i` is:


$$
X_i
=
[\, 
z(\mathbf{FPCA}_i),\;
z(\mathbf{Season}_i)
\,]
$$

Here, 
$$
z(x)=\frac{x-\mu}{\sigma}
$$

Both parts currently use weight `1`:

$$
X_i = [\,1 \cdot \mathbf{FPCA}_i,\; 1 \cdot \mathbf{Season}_i\,]
$$

The previous FPCA-only analysis can still be reproduced with:

```bash
FEATURE_SET=fpca
```

---

## ClustGeo Augmented

Run label:

```text
clustergeoaug
```

ClustGeo combines feature-space distance and geographic distance.

Let:

$$
D_0(i,j) = d(X_i, X_j)
$$

$$
D_1(i,j) = d_G(i,j)
$$

where $D_0(i,j)$ is the distance between augmented flu feature vectors and
$D_1(i,j)$ is the geographic distance between county centroids.

The mixed distance is:

$$
D_{\alpha}(i,j) = (1 - \alpha)D_0(i,j) + \alpha D_1(i,j)
$$

Current setting:

$$
\alpha = 0.2
$$

Interpretation:

- `80%` of the distance comes from augmented flu dynamics
- `20%` comes from geographic distance

The mixed distance matrix is clustered with Ward hierarchical clustering, then
the tree is cut at the requested `K`.

---

## REDCAP Augmented

Run label:

```text
redcapaug
```

The current REDCAP-style implementation uses the same augmented feature matrix,
but spatial structure is treated as a hard adjacency constraint.

Let `A(i,j)` be the adjacency indicator:

$$
A(i,j) = 1
$$

for adjacent counties, and

$$
A(i,j) = 0
$$

otherwise.

The feature distance is:

$$
D_0(i,j) = d(X_i, X_j)
$$

A large penalty is assigned to non-adjacent county pairs:

$$
D_R(i,j) = D_0(i,j)
$$

when `A(i,j) = 1`, and

$$
D_R(i,j) = M
$$

when `A(i,j) = 0`.

where $M$ is a very large finite penalty:

$$
M = 10000 \times \max_{i,j} D_0(i,j)
$$

Ward hierarchical clustering is then applied to this spatially penalized
distance matrix, and the tree is cut at the requested `K`.

Interpretation:

- ClustGeo allows a smooth tradeoff between flu similarity and geographic
  compactness.
- REDCAP augmented imposes a much stronger spatial adjacency penalty.

---

## Spatial Variation Metric

Spatial variation evaluates how much county-level heterogeneity is preserved by
a regional aggregation.

For county `i`, region `g(i)`, week `t`, and weight `w_i`, define:

- $y_{i,t}$: county flu ED visit proportion
- $y_{g(i),t}$: weighted regional mean for county `i`
- $y_{S,t}$: weighted statewide mean

The population-weighted within-region variation is:

$$
W_{R,t} = \sum_i w_i (y_{i,t} - y_{g(i),t})^2
$$

The population-weighted statewide variation is:

$$
W_{S,t} = \sum_i w_i (y_{i,t} - y_{S,t})^2
$$

The weekly retained spatial variation is:

$$
\lambda_{K,t}
= 1 - \frac{W_{R,t}}{W_{S,t}}
$$

The reported value is the average across flu-season weeks and test seasons:

$$
\lambda_K = \frac{1}{T}\sum_{t=1}^{T}\lambda_{K,t}
$$

Interpretation:

- $\lambda_K = 0$: same as state-level aggregation
- $\lambda_K = 1$: same as county-level resolution
- Higher values mean the aggregation preserves more county-level spatial
  heterogeneity

---

## Spatial Variation Figures

### ClustGeo Augmented

![ClustGeo augmented spatial variation](../figures/summary/spatial_variation_clustergeoaug.png)

### REDCAP Augmented

![REDCAP augmented spatial variation](../figures/summary/spatial_variation_redcapaug.png)

---

## Overall Spatial Variation Results

Values below are population-weighted, restricted to flu-season months, and
averaged across the evaluated test seasons.

### Existing Boundary Benchmarks

| Boundary | $K$ | $\lambda_K$ |
|---|---:|---:|
| State | 1 | 0.000 |
| DSHS | 8 | 0.462 |
| RAC | 22 | 0.601 |
| HSA | 61 | 0.754 |
| County | 254 | 1.000 |

### Data-Driven Cluster Methods

| $K$ | ClustGeo Augmented | REDCAP Augmented | Difference |
|---:|---:|---:|---:|
| 7 | 0.364 | 0.327 | 0.037 |
| 9 | 0.390 | 0.363 | 0.027 |
| 15 | 0.512 | 0.492 | 0.020 |
| 21 | 0.581 | 0.523 | 0.059 |
| 23 | 0.590 | 0.540 | 0.050 |
| 31 | 0.646 | 0.565 | 0.081 |
| 45 | 0.705 | 0.661 | 0.044 |
| 61 | 0.744 | 0.694 | 0.049 |

ClustGeo augmented retained more spatial variation than REDCAP augmented for
every tested $K$.

---

## Representative Cluster Region Figures

The figures below combine a county cluster map with the corresponding cluster
trend plot. The examples use the `2024/25` excluded season.

Full cluster figures for all tested seasons and `K` values are available here:

[../figures/cluster_combine](../figures/cluster_combine)

### K = 23

ClustGeo augmented:

![ClustGeo augmented, exclude 2024/25, K=23](../figures/cluster_combine/county_clustergeoaug_alpha0p2_exclude_2024-25_k23.png)

REDCAP augmented:

![REDCAP augmented, exclude 2024/25, K=23](../figures/cluster_combine/county_redcapaug_exclude_2024-25_k23.png)

### K = 61

ClustGeo augmented:

![ClustGeo augmented, exclude 2024/25, K=61](../figures/cluster_combine/county_clustergeoaug_alpha0p2_exclude_2024-25_k61.png)

REDCAP augmented:

![REDCAP augmented, exclude 2024/25, K=61](../figures/cluster_combine/county_redcapaug_exclude_2024-25_k61.png)

---

## Interpretation

From spatial variation alone:

- ClustGeo augmented exceeds DSHS by $K = 15$.
- ClustGeo augmented is close to RAC by $K = 23$.
- ClustGeo augmented approaches HSA at $K = 61$.
- REDCAP augmented improves as `K` increases, but it is lower than ClustGeo
  augmented at every tested $K$.

This does not determine the final clustering choice. Spatial variation measures
how much local heterogeneity is preserved, but it does not measure forecast
skill. The final comparison should combine this table with WIS from the
forecasting outputs.

Recommended forecasting labels:

```text
clustergeoaug
redcapaug
```

Example forecast command:

```bash
python code/forecasting/run_forecast.py \
  --forecast_date 2024-01-06 \
  --method_name clustergeoaug \
  --k_list 7,9,15,21,23,31,45,61 \
  --n_workers 8
```
