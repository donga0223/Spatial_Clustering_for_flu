# Spatial Clustering for Influenza Forecasting

## Overview

Influenza dynamics vary substantially across geographic regions due to differences in population density, mobility, healthcare utilization, and reporting patterns.

This project evaluates whether data-driven geographic aggregation can better characterize influenza dynamics compared to existing administrative or healthcare-based geographic boundaries (e.g., county, HSA, RAC, DSHS, state).

The primary goals of this project are:

* To identify geographically coherent regions with similar influenza activity patterns
* To quantify how well different geographic aggregations preserve spatial heterogeneity
* To evaluate forecasting performance across multiple geographic scales
* To understand the tradeoff between spatial resolution and forecasting accuracy

---

## Project Structure

The main code is organized into four folders under `code/`.

```bash
code/
├── clustering/
├── spatial_variation/
├── forecasting/
└── evaluation/
```

## Evaluation Metrics

Forecasting performance is evaluated using:

* **Coverage**: Prediction interval coverage
* **MAE**: Mean Absolute Error
* **WIS**: Weighted Interval Score

---

## Geographic Levels

This project compares forecasting and spatial variation across:

* County
* HSA
* Cluster-based aggregation
* RAC
* DSHS Region
* State


## Batch Submission

Forecasts can be submitted across multiple forecast dates using the Slurm launcher script.

Example for running the `redcap` method for the 2025–26 season:

```bash
for d in 2025-10-04 2025-10-11 2025-10-18 2025-10-25 \
  2025-11-01 2025-11-08 2025-11-15 2025-11-22 2025-11-29 \
  2025-12-06 2025-12-13 2025-12-20 2025-12-27 \
  2026-01-03 2026-01-10 2026-01-17 2026-01-24 2026-01-31 \
  2026-02-07 2026-02-14 2026-02-21 2026-02-28 \
  2026-03-07 2026-03-14 2026-03-21 2026-03-28; do

  sbatch --export=METHOD=redcap,DATE=$d run_array_launcher.sh

done
```

To run a different clustering method, replace `redcap` with one of:

* `clustergeo`
* `skater`
* `redcap`

The launcher script runs forecasts for odd values of K from 5 to 45.
