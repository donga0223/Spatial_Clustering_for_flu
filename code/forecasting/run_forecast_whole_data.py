import argparse
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

sys.path.append(str(Path(__file__).parent))

import pandas as pd
import multiprocessing

import loader
import preprocess_and_plot
import forecast_model
from timeseriesutils import featurize


TX_RAC = pd.read_csv("data/tx_rac.csv")
TX_dshs = pd.read_csv("data/tx_dshs_region.csv")


def add_target_end_date(dat):
    if "target_end_date" in dat.columns:
        dat["target_end_date"] = pd.to_datetime(dat["target_end_date"])
        return dat

    if "Date" not in dat.columns:
        raise ValueError("Input data must include either target_end_date or Date.")

    dat["target_end_date"] = pd.to_datetime(dat["Date"]) + pd.Timedelta(days=6)
    return dat


def build_forecast_units(dat):
    dat = dat.copy()
    dat["cluster"] = "G_" + dat["cluster"].astype(str)

    df_cluster = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["cluster"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_cluster["geo_level"] = "cluster"

    dat["state"] = "TX"
    df_state = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["state"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_state["geo_level"] = "state"

    df_county = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["county"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_county["geo_level"] = "county"

    dat_rac = dat.merge(TX_RAC, left_on="county", right_on="County", how="left")
    df_rac = preprocess_and_plot.cal_inc_by_group(
        dat=dat_rac,
        group_cols=["RAC"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_rac["geo_level"] = "rac"

    dat_dshs = dat.merge(TX_dshs, left_on="county", right_on="county", how="left")
    df_dshs = preprocess_and_plot.cal_inc_by_group(
        dat=dat_dshs,
        group_cols=["dshs_region"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_dshs["geo_level"] = "dshs"

    df_hsa = preprocess_and_plot.cal_inc_by_group(
        dat=dat,
        group_cols=["hsa_nci_id"],
        date_col="target_end_date",
        flu_col="value_flu",
        all_col="value_all",
        pop_col="population",
        loader=loader,
    )
    df_hsa["geo_level"] = "hsa"

    return pd.concat(
        [df_cluster, df_county, df_hsa, df_rac, df_dshs, df_state],
        axis=0,
    )


def run_single_forecast(k, forecast_date, method_name, cluster_data_dir, output_root):
    print(f"[START] method={method_name}, k={k}, forecast_date={forecast_date}", flush=True)

    input_file = Path(cluster_data_dir) / f"df_county_{method_name}_{k}.csv"
    if not input_file.exists():
        print(f"[SKIP] File not found: {input_file}", flush=True)
        return

    dat = pd.read_csv(input_file)
    dat = add_target_end_date(dat)

    df_final = build_forecast_units(dat)

    ref_date = forecast_date + timedelta((5 - forecast_date.weekday()) % 7)
    print(f"  [k={k}] Reference date = {ref_date}", flush=True)

    df_shifted = preprocess_and_plot.shift_future_seasons(df_final, ref_date)
    df_shifted_trainable = df_shifted[
        df_shifted["wk_end_date"] < pd.Timestamp(ref_date)
    ].copy()

    transform_df = preprocess_and_plot.transform_incidence(df_shifted_trainable)

    df, feat_names = preprocess_and_plot.build_features(
        transform_df,
        featurize,
        ref_date,
    )

    q_levels = [0.025, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.975]
    q_labels = ["0.025", "0.05", "0.1", "0.25", "0.5",
                "0.75", "0.9", "0.95", "0.975"]

    preds_df, _ = forecast_model.generate_quantile_forecasts(
        df=df,
        feat_names=feat_names,
        q_levels=q_levels,
        q_labels=q_labels,
        num_bags=50,
        bag_frac_samples=1,
        ref_date=ref_date,
    )

    output_path = Path(output_root) / "whole" / f"TX_NSSP_county_{method_name}_{k}_pct"
    output_path.mkdir(parents=True, exist_ok=True)

    output_file = output_path / f"{ref_date}-GBQR.csv"
    preds_df.to_csv(output_file, index=False)

    print(f"[DONE] Saved: {output_file}", flush=True)


def parse_k_list(k_list):
    if k_list is None or k_list.strip() == "":
        return None

    parsed = sorted({
        int(k.strip())
        for k in k_list.split(",")
        if k.strip() != ""
    })

    if not parsed:
        raise ValueError("--k_list must contain at least one integer K value.")

    return parsed


def run_all_k_parallel(forecast_date, method_name, cluster_data_dir, output_root,
                       k_min=5, k_max=45, k_list=None, n_workers=None):
    if k_list is None:
        k_list = list(range(k_min, k_max + 1, 2))

    if n_workers is None:
        n_workers = len(k_list)

    args = [
        (k, forecast_date, method_name, cluster_data_dir, output_root)
        for k in k_list
    ]

    print(
        f"Running whole-data forecast for k={','.join(map(str, k_list))} "
        f"in parallel ({n_workers} workers), method={method_name}, "
        f"date={forecast_date}",
        flush=True,
    )

    with multiprocessing.Pool(processes=n_workers) as pool:
        pool.starmap(run_single_forecast, args)

    print(f"[ALL DONE] method={method_name}, date={forecast_date}", flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--forecast_date", type=str, required=True)
    parser.add_argument("--method_name", type=str, required=True,
                        help="Whole-data method label, e.g. clustergeo or clustergeoaug.")
    parser.add_argument("--cluster_data_dir", type=str, default="data/cluster_data")
    parser.add_argument(
        "--output_root",
        type=str,
        default=os.getenv(
            "MODEL_OUTPUT_DIR",
            "/work2/09967/dongahkim0223/frontera/Spatial_clustering/model_output",
        ),
    )
    parser.add_argument("--k_min", type=int, default=5)
    parser.add_argument("--k_max", type=int, default=45)
    parser.add_argument("--k_list", type=str, default=None,
                        help="Comma-separated selected K values, e.g. 8,15,22,30,40,50,61,65.")
    parser.add_argument("--n_workers", type=int, default=None)

    args = parser.parse_args()

    forecast_date = datetime.strptime(args.forecast_date, "%Y-%m-%d").date()

    run_all_k_parallel(
        forecast_date=forecast_date,
        method_name=args.method_name,
        cluster_data_dir=args.cluster_data_dir,
        output_root=args.output_root,
        k_min=args.k_min,
        k_max=args.k_max,
        k_list=parse_k_list(args.k_list),
        n_workers=args.n_workers,
    )
