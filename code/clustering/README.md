# Spatial Clustering for Influenza Dynamics

## Overview

This folder contains scripts for spatial clustering of influenza dynamics across Texas counties and HSAs.

The goal is to group geographically contiguous regions with similar influenza temporal patterns using FPCA-based features and spatially constrained clustering methods. These cluster assignments are later used for influenza forecasting and evaluation.

Current clustering methods implemented:
- REDCAP
- ClustGeo
- Skater

Current primary method:
- REDCAP

---

## File Structure

Main clustering scripts are located in:

```text
code/clustering/
```

Key files:

### 1. `fPCA_contiguous_function.R`

This file contains core functions used for feature extraction and spatial clustering.

Main functions include:

- `get_pc_scores()`
  - Original FPCA feature extraction function.
  - Applies rolling mean smoothing over the full time series.

- `get_pc_scores_seasonwise()`
  - Modified FPCA feature extraction.
  - Applies smoothing within each season separately to avoid season boundary artifacts.

- `run_redcap_cluster()`
  - Performs REDCAP clustering using FPCA features and spatial adjacency.

- `run_clustgeo_cluster()`
  - Performs ClustGeo clustering.

- `run_skater_cluster()`
  - Performs Skater clustering.

This file mainly contains reusable functions.

---

### 2. `cluster_data_by_season.R`

This is the main execution script for seasonal clustering.

Main responsibilities:

1. Load county-level flu ED visit data.
2. Load spatial shapefiles.
3. Loop over target seasons.
4. Exclude target season from training.
5. Compute FPCA scores.
6. Run clustering for multiple K values.
7. Save cluster assignments.
8. Save diagnostic plots.

This file controls the full clustering pipeline.

---

Workflow relationship:

```text
cluster_data_by_season.R
        ↓
calls functions from
        ↓
fPCA_contiguous_function.R
```

code/clustering/
├── cluster_data_by_season.R
├── fPCA_contiguous_function.R


## Input Data

### Main Dataset

`data/county_edvisits.csv`

Main columns:

Main columns:

| Column | Description |
|--------|-------------|
| season | Influenza season (e.g., 2023/24) |
| Date | Weekly date |
| hsa_nci_id | HSA identifier |
| county | County name |
| value_flu | Number of flu-related ED visits |
| value_all | Total ED visits |
| value | Proportion of flu-related ED visits (`value_flu / value_all`) |
| population | County population |

---

## Clustering Workflow

### Step 1. Load data
Load county-level influenza ED visit data and geographic shapefiles.

### Step 2. Leave-One-Season-Out training
To prevent data leakage, the target test season is excluded when constructing cluster boundaries.

Example:
- Test season: 2024/25
- Training seasons: all remaining seasons

```r
df_train_seasons <- df_long %>%
  filter(season != sea)
```

---

### Step 3. Restrict to influenza season
Training data is restricted to peak influenza months:

- October
- November
- December
- January
- February
- March

```r
df_train_in_season <- df_train_seasons %>%
  mutate(Date_parsed = as.Date(Date)) %>%
  filter(month(Date_parsed) %in% c(10, 11, 12, 1, 2, 3))
```

---

### Step 4. Compute FPCA features

FPCA (Functional Principal Component Analysis) is used to summarize temporal influenza patterns.

Each county receives FPCA scores that represent major temporal patterns such as:
- Peak timing
- Peak magnitude
- Growth rate
- Decline speed

These FPCA scores are used as clustering features.

---

### Step 5. Spatial clustering

Clustering is performed with spatial contiguity constraints.

Methods available:
- REDCAP
- ClustGeo
- Skater

The clustering produces geographically contiguous regions with similar influenza dynamics.

---

### Step 6. Save outputs

Outputs include:
- Cluster assignments
- Diagnostic maps
- Time-series plots

---

## Season-wise Smoothing

To avoid boundary artifacts between influenza seasons, smoothing is applied within each season separately before FPCA.

This prevents artificial smoothing across:
- March (end of season)
- October (start of next season)

Example:
- Good: smooth only within 2024/25 season
- Avoid: smoothing across 2024/25 March → 2025/26 October

---

## Output Files

### Cluster assignments

Location:
`data/cluster_data_season/`

Example files:
- `df_county_redcap_exclude_2024-25_5.csv`
- `df_county_redcap_exclude_2024-25_7.csv`

Naming convention:

`df_<level>_<method>_exclude_<season>_<k>.csv`

Where:
- level = county or hsa
- method = redcap / clustergeo / skater
- season = excluded test season
- k = number of clusters

---

### Figures

Location:
`figures/cluster_combine/`

Saved outputs include:
- Cluster maps
- Cluster trend plots
- Combined diagnostic plots

---

## Notes

### County-level clustering
- K values tested: 5–45 (odd numbers only)

### HSA-level clustering
- K values tested: 2–22

### Current default settings
- Method: REDCAP
- FPCA variance threshold: 95%
- Training period: October–March