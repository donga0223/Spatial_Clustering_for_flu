# Spatial Variation Analysis

This folder contains code for calculating **spatial variation preservation** across multiple geographical aggregation levels.

The goal is to quantify how much spatial heterogeneity at the county level is preserved after aggregating counties into larger regions.

Aggregation levels evaluated:

* HSA
* RAC
* DSHS Region
* Cluster-based regions

This analysis is used to compare how well different regional aggregation methods preserve local spatial structure.

---

## Main Scripts

| File                            | Purpose                                                                         |
| ------------------------------- | ------------------------------------------------------------------------------- |
| `spatial_variation_functions.R` | Core helper functions for loading data, computing lambda, and compiling results |
| `run_spatial_variation.R`       | Main script for running spatial variation analysis                              |
| `README.md`                     | Documentation                                                                   |

---

## Workflow

```text
cluster_data_season/*_all.csv
    ↓
load observed held-out test season data
    ↓
build geographic mapping table
    ↓
compute spatial variation preservation (lambda)
    ↓
summarize by season
    ↓
compute overall summary across test seasons
    ↓
generate figures
```

---

## Main Inputs

Input files are generated from the clustering pipeline.

```text
data/cluster_data_season/*_all.csv
```

Examples:

```text
df_county_redcap_exclude_2024-25_5_all.csv
df_county_clustergeo_exclude_2025-26_15_all.csv
```

Metadata files:

```text
data/tx_rac.csv
data/tx_dshs_region.csv
data/tx_hsa.csv
```

These files are used to map counties into:

* RAC
* DSHS Region
* HSA
* Cluster

---

## Main Outputs

Generated outputs include:

* spatial variation summary tables
* overall comparison figures
* test-season-specific comparison figures
* optional `.rds` result files

Examples:

```text
figures/spatial_variation_all_cases.pdf
figures/spatial_variation_results.rds
```

---

## Metric Definition

Spatial variation preservation is measured using:

```text
λ = 1 - (within-region variance / state-level variance)
```

where:

* **within-region variance** measures county-level variation relative to aggregated regions
* **state-level variance** measures county-level variation relative to the state average

Interpretation:

* λ close to **1** → aggregation preserves spatial heterogeneity well
* λ close to **0** → aggregation preserves little spatial structure
* larger λ indicates better preservation of county-level variation

---

## Weighting Schemes

Two versions are supported.

### 1. Unweighted

Each county contributes equally.

Used to evaluate raw spatial heterogeneity preservation.

---

### 2. Weighted

Counties are weighted by importance.

Current supported weighting:

* population

Future extensions may include:

* ED visit volume
* custom weights

Weighted analysis gives more influence to larger or more important counties.

---

## Analysis Setup

```text
Input unit: County
Comparison levels: HSA, RAC, DSHS, Cluster
Periods:
    - Full Period
    - Flu Season Months
Flu season months:
    October–March
```

---

## Output Structure

Results contain:

* `method`
* `K`
* `geo_level`
* `period_type`
* `type`
* `season`
* `test_season`
* `lambda_K`
* `n_weeks`
* `weight_type`

### Result Types

#### by_season

Lambda computed using one held-out test season.

Example:

```text
test_season = 2024-25
```

#### overall_across_test_seasons

Average lambda across all held-out test seasons.

Example:

```text
test_season = Overall
```

---

## Notes

* County is the base spatial unit.
* All comparisons are evaluated relative to county-level variation.
* Overall summaries are computed only during the final compilation step.
* Intermediate functions only return season-specific results.
