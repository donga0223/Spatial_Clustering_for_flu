# Evaluation Module

This folder contains scripts for evaluating influenza forecasting performance across multiple geographic aggregation levels (e.g., county, HSA, cluster, RAC, DSHS, state), as well as scripts for generating summary and trajectory figures.

## Main Components

### Core evaluation functions

* **forecast_evaluation_functions.R**
  Contains core functions for:

  * calculating evaluation metrics (Coverage, MAE, WIS)
  * comparing forecasts across geographic levels
  * summarizing results for downstream visualization

### Summary figure functions

* **forecasting_summary_figure_function.R**
  Functions for generating summary figures of forecasting performance across:

  * geographic levels
  * cluster sizes (K)
  * evaluation metrics

### Trajectory figure functions

* **forecasting_trajectory_figure_function.R**
  Functions for plotting trajectory-based figures, including:

  * forecast vs observed trajectories
  * location-specific temporal patterns
  * selected geographic comparisons

---

## Run Scripts

### Run evaluation

* **run_forecast_evaluation.R**
  Main script for running forecasting evaluation.
  Outputs evaluation results and summary metric tables.

### Run summary figures

* **run_summary_figure.R**
  Generates summary performance figures from evaluation outputs.

### Run trajectory figures

* **run_trajectory_figure.R**
  Generates trajectory plots for selected locations and aggregation levels.

---

## Workflow

Recommended execution order:

1. Run evaluation : compute & save tables
   `run_forecast_evaluation.R`

2. Generate summary figures : load tables & plot summary
   `run_summary_figure.R`

3. Generate trajectory figures : load tables & plot trajectory
   `run_trajectory_figure.R`

---

## Evaluation Metrics

Main metrics used in this analysis:

* **Coverage**: prediction interval coverage
* **MAE**: Mean Absolute Error
* **WIS**: Weighted Interval Score

---

## Geographic Levels

Evaluation includes the following geographic levels:

* County
* HSA
* Cluster-based aggregation
* RAC
* DSHS Region
* State
