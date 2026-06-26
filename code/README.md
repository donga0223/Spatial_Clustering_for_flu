## Code Structure

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
