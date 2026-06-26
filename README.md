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

### 1. clustering/

This folder contains scripts for:

* performing spatial clustering
* generating cluster assignments for different numbers of clusters (K)
* preparing cluster-level datasets for downstream analysis

Main outputs:

* cluster assignments
* cluster-level processed datasets

---

### 2. spatial_variation/

This folder contains scripts for:

* calculating spatial variation metrics
* evaluating how much spatial heterogeneity is preserved under each geographic aggregation
* generating spatial variation summary figures

Main outputs:

* spatial variation metrics
* spatial variation figures

---

### 3. forecasting/

This folder contains scripts for:

* training forecasting models
* generating influenza forecasts at different geographic aggregation levels
* saving forecast outputs for downstream evaluation

Main outputs:

* forecast results
* prediction intervals
* model outputs

---

### 4. evaluation/

This folder contains scripts for:

* evaluating forecasting performance using forecast outputs
* calculating evaluation metrics (Coverage, MAE, WIS)
* generating summary and trajectory figures

Main outputs:

* evaluation tables
* summary metrics
* performance figures

---

## Workflow

Recommended execution order:

### Step 1: Clustering

Run clustering pipeline and generate datasets for downstream analysis.

```bash
code/clustering/
```

---

### Step 2: Spatial Variation Analysis

Calculate spatial variation metrics and generate related figures.

```bash
code/spatial_variation/
```

---

### Step 3: Forecasting

Run forecasting models across geographic aggregation levels.

```bash
code/forecasting/
```

---

### Step 4: Evaluation

Evaluate forecast performance and generate summary figures.

```bash
code/evaluation/
```

---

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
