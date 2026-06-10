import argparse
import sys
sys.path.append("code")
import os
from pathlib import Path
from datetime import datetime, timedelta

import numpy as np
import pandas as pd
import lightgbm as lgb
import pymmwr
import multiprocessing

import loader
import preprocess_and_plot
import forecast_model
from timeseriesutils import featurize

# RAC 
TX_RAC = pd.read_csv('data/tx_rac.csv')

# ==============================================================================
# Core function: run forecast for a single (k, forecast_date, method_name)
# ==============================================================================
def run_single_forecast(k, forecast_date, method_name):
    """
    Run forecast for one combination of k, forecast_date, method_name.
    Can be called directly or via multiprocessing.Pool.
    """
    print(f"[START] method={method_name}, k={k}, forecast_date={forecast_date}", flush=True)

    # ---------------------------
    # DATA LOAD
    # ---------------------------
    input_file = f"data/cluster_data/df_county_{method_name}_{k}.csv"
    if not os.path.exists(input_file):
        print(f"[SKIP] File not found: {input_file}", flush=True)
        return

    dat = pd.read_csv(input_file)
    dat["cluster"] = "G_" + dat["cluster"].astype(str)

    # Cluster level
    df_cluster = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["cluster"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader
    )
    df_cluster["geo_level"] = "cluster"

    # State level
    dat["state"] = "TX"
    df_state = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["state"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader
    )
    df_state["geo_level"] = "state"

    # County level
    df_county = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["county"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader
    )
    df_county["geo_level"] = "county"

    # RAC level
    dat2 = dat.merge(TX_RAC, left_on="county", right_on="County", how="left")
    df_rac = preprocess_and_plot.cal_inc_by_group(
        dat=dat2,
        group_cols=["RAC"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader
    )
    df_rac["geo_level"] = "rac"

    df_final = pd.concat([df_cluster, df_county, df_rac, df_state], axis=0)
    
    # ---------------------------
    # FORECAST
    # ---------------------------
    ref_date = forecast_date + timedelta(
        (5 - forecast_date.weekday()) % 7
    )
    print(f"  [k={k}] Reference date = {ref_date}", flush=True)

    df_shifted = preprocess_and_plot.shift_future_seasons(df_final, ref_date)
    transform_df = preprocess_and_plot.transform_incidence(df_shifted)


    df, feat_names = preprocess_and_plot.build_features(
        transform_df,
        featurize,
        ref_date
    )

    q_levels = [0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975]
    q_labels  = ['0.025', '0.05', '0.1', '0.25', '0.5',
                 '0.75', '0.9', '0.95', '0.975']

    preds_df, _ = forecast_model.generate_quantile_forecasts(
        df=df,
        feat_names=feat_names,
        q_levels=q_levels,
        q_labels=q_labels,
        num_bags=100,
        bag_frac_samples=1,
        ref_date=ref_date
    )

    # ---------------------------
    # SAVE
    # ---------------------------
    root = Path.cwd()
    output_path = root / "model_output" / f"TX_NSSP_county_{method_name}_{k}_pct"
    output_path.mkdir(parents=True, exist_ok=True)

    output_file = output_path / f"{ref_date}-GBQR.csv"
    preds_df.to_csv(output_file, index=False)

    print(f"[DONE] Saved: {output_file}", flush=True)


# ==============================================================================
# Parallel runner: run all k values for one forecast_date in parallel
# ==============================================================================
def run_all_k_parallel(forecast_date, method_name,
                       k_min=5, k_max=25, n_workers=None):
    """
    Run forecast for all k (k_min to k_max) in parallel using multiprocessing.
    """
    k_list = list(range(k_min, k_max + 1))
    if n_workers is None:
        n_workers = len(k_list)

    args = [(k, forecast_date, method_name) for k in k_list]

    print(f"Running k={k_min}~{k_max} in parallel ({n_workers} workers), "
          f"method={method_name}, date={forecast_date}", flush=True)

    with multiprocessing.Pool(processes=n_workers) as pool:
        pool.starmap(run_single_forecast, args)

    print(f"[ALL DONE] method={method_name}, date={forecast_date}", flush=True)


# ==============================================================================
# Entry point
# ==============================================================================
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--forecast_date", type=str, required=True)
    parser.add_argument("--method_name",   type=str, required=True,
                        help="skater, clustergeo, or redcap")
    parser.add_argument("--k_min",         type=int, default=5)
    parser.add_argument("--k_max",         type=int, default=25)
    parser.add_argument("--n_workers",     type=int, default=None,
                        help="Number of parallel workers. Defaults to number of k values.")

    args = parser.parse_args()

    forecast_date = datetime.strptime(args.forecast_date, "%Y-%m-%d").date()

    run_all_k_parallel(
        forecast_date = forecast_date,
        method_name   = args.method_name,
        k_min         = args.k_min,
        k_max         = args.k_max,
        n_workers     = args.n_workers
    )
