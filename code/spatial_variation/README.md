# Spatial Variation Analysis

This folder contains code for calculating spatial variation preserved across different geographical aggregation levels.

The goal is to quantify how much spatial heterogeneity is preserved after aggregation.

Spatial variation is evaluated for:
- HSA
- RAC
- DSHS Region
- Cluster-based regions

The outputs are later used for:
- clustering comparison
- model evaluation
- summary figures for analysis and manuscript

---

## Main Scripts

| File | Purpose |
|------|---------|
| `spatial_variation_functions.R` | Helper functions for spatial variation calculation and plotting |
| `run_spatial_variation.R` | Main script for running spatial variation analysis |
| `README.md` | Documentation |

---

## Quick Workflow

```text
cluster_data_season/*.csv
    ↓
load observed test season data
    ↓
build geographic mapping table
    ↓
calculate spatial variation
    ↓
compute overall summaries
    ↓
generate figures
```

---

## Main Inputs

Main input files are generated from the clustering pipeline:

```text
data/cluster_data_season/*.csv
data/cluster_data_season/*_all.csv
```

Examples:

```text
df_county_redcap_exclude_2024-25_5.csv
df_county_redcap_exclude_2024-25_5_all.csv
```

Metadata files:

```text
data/tx_rac.csv
data/tx_dshs_region.csv
data/tx_hsa.csv
```

These are used to map counties to:
- RAC
- DSHS Region
- HSA
- Cluster

---

## Main Outputs

Generated outputs include:

- spatial variation summary tables
- overall comparison figures
- season-specific comparison figures

Example outputs:

```text
figures/lambda_overall_test_only.png
figures/lambda_by_test_season.png
figures/spatial_variation_all_cases.pdf
```

---

## Spatial Variation Metrics

Two versions of spatial variation are considered.

### 1. Unweighted Spatial Variation
Each county contributes equally.

Used to evaluate overall preservation of spatial heterogeneity.

---

### 2. Weighted Spatial Variation
Spatial variation is weighted by regional importance.

Possible weighting schemes:
- population
- ED visit volume
- other custom weights

This helps reduce sensitivity to noisy small regions and gives more importance to large influential regions.

---

## Current Recommended Setup

```text
Input unit: County
Comparison levels: HSA, RAC, DSHS, Cluster
Periods: Full period + Peak season only
Peak months: October–March
```

---

## Documentation

Detailed documentation:

- [Workflow](docs/workflow.md)
- [Metric definition](docs/metrics.md)
- [Output files](docs/output_files.md)
- [Design choices](docs/design_choices.md)