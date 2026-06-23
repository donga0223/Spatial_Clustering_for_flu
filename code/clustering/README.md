# Clustering Pipeline

This folder contains the code for creating spatial clusters of Texas counties and HSAs based on influenza ED visit dynamics.

The clustering outputs are used later for forecasting, summary metrics, and evaluation.

---

## Main Scripts

| File | Purpose |
|------|---------|
| `cluster_data_by_season.R` | Main script to generate leave-one-season-out clustering files |
| `fPCA_contiguous_function.R` | Helper functions for FPCA feature extraction, spatial clustering, and plotting |
| `save_all_csv.ipynb` | Combines season-specific clustering files into `_all.csv` files |
| `clean_data.R` | Data cleaning script |
| `cluster_data.R` | Legacy clustering script. Uses full-year data and does not apply leave-one-season-out training |
| `save_av.ipynb` | Helper notebook for creating animation/video outputs |

---

## Quick Workflow

```text
county_edvisits.csv
    ↓
exclude one test season
    ↓
restrict to flu season months
    ↓
compute FPCA features
    ↓
run spatial clustering
    ↓
save season-specific cluster files
    ↓
combine files into *_all.csv
```

The generated `_all.csv` files are used as inputs for downstream forecasting and evaluation pipelines.

---

## Main Outputs

Season-specific clustering outputs are saved as:

```text
data/cluster_data_season/df_county_<method>_exclude_<season>_<k>.csv
data/cluster_data_season/df_hsa_<method>_exclude_<season>_<k>.csv
```

Combined files are saved as:

```text
data/cluster_data_season/df_county_<method>_all.csv
data/cluster_data_season/df_hsa_<method>_all.csv
```

The `_all.csv` files are used later for forecasting, summary metrics, and evaluation.

---

## Documentation

More detailed notes are stored in:

- [Workflow](docs/workflow.md)
- [FPCA feature extraction](docs/fpca.md)
- [Clustering methods](docs/methods.md)
- [Output files](docs/output_files.md)
- [Design choices](docs/design_choices.md)

---

## Current Recommended Setup

```text
Unit levels: county, HSA
Primary method: REDCAP
Training design: leave-one-season-out
Training window: October–March
Feature extraction: season-wise smoothed FPCA
County K values: 5–45, odd numbers only
HSA K values: 2–22
```