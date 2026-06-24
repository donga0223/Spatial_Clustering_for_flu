# Forecasting 

This folder contains code for generating influenza forecasts across multiple geographical aggregation levels in Texas.
The goal is to forecast future Flu ED visit percentages at different spatial resolutions and compare forecasting performance across aggregation strategies.

Aggregation levels included:

* County
* HSA
* RAC
* DSHS Region
* Cluster-based regions
* State

This analysis is used to evaluate whether regional aggregation improves forecasting performance while preserving local dynamics.

---

## Main Scripts

| File                     |  Purpose                                                                               |
| ------------------------ | ------------------------------------------------------------------------------------- |
| `run_forecast.py`        | Main script for running forecasts for a given forecast date, clustering method, and K |
| `forecast_model.py`      | Core LightGBM quantile regression forecasting model                                   |
| `preprocess_and_plot.py` | Data aggregation, preprocessing, feature generation, and transformations              |
| `loader.py`              | Helper functions for epiweek, season week, holiday, and season handling               |
| `README.md`              | Documentation                                                                         |

## Workflow

```text
cluster_data_season/*.csv
    ↓
load cluster-level input data
    ↓
aggregate to geographic levels
(county / cluster / HSA / RAC / DSHS / state)
    ↓
preprocess incidence
    ↓
build forecasting features
    ↓
train quantile regression models
    ↓
generate probabilistic forecasts
    ↓
save forecast outputs
```

---

## Main Inputs

Input files are generated from the clustering pipeline.

```text
data/cluster_data_season/*.csv
```

Examples:

```text
df_county_redcap_exclude_2024-25_5.csv
df_county_clustergeo_exclude_2025-26_15.csv
```

Metadata files:

```text
data/tx_rac.csv
data/tx_dshs_region.csv
```

These files are used to map counties into:

* RAC
* DSHS Region
* HSA (already included in data/cluster_data_season/*.csv)
* Cluster

---

## Main Outputs

Generated outputs include:

* forecast CSV files
* probabilistic quantile forecasts
* model predictions across all aggregation levels

Examples:

```text
model_output/season/TX_NSSP_county_clustergeo_2024-25_5_pct/
model_output/season/TX_NSSP_county_clustergeo_2024-25_5_pct/2024-10-05-GBQR.csv
```

---

## Forecast Model

Forecasts are generated using:

* LightGBM Quantile Regression
* Bagged ensemble training
* Multi-quantile prediction

Output target:

```text
Flu ED visits pct
```

---

## Feature Engineering

Features include:

* transformed incidence
* season week
* log population
* holiday effects
* lag features
* rolling mean features
* Taylor approximation features
* location indicators

Future-season data can optionally be shifted backward to increase training data availability.

---

## Analysis Setup

```test
Input unit: County-level cluster dataset
Comparison levels:
    - County
    - Cluster
    - HSA
    - RAC
    - DSHS
    - State
Forecast target:
    - Flu ED visits pct
```

Supported clustering methods:
* clustergeo
* redcap
* skater

---

## Notes

* County is the base spatial unit.
* Geographic aggregation is computed using:

```text
inc = sum(value_flu) / sum(value_all)
```

* Forecasts are probabilistic rather than point estimates.
* Outputs are saved separately for each:
    * clustering method
    * number of clusters (K)
    * forecast date
