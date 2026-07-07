#!/bin/bash
#SBATCH -J gbqr_county
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=21
#SBATCH -t 12:00:00
#SBATCH -o logs/slurm_%j.out
#SBATCH -e logs/slurm_%j.err

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

source ~/miniconda3/etc/profile.d/conda.sh
conda activate flusion

cd ~/Spatial_clustering

K_LIST="${K_LIST:-7,9,15,21,23,31,45,61}"
N_WORKERS="${N_WORKERS:-8}"

echo "Running METHOD=$METHOD, DATE=$DATE, K_LIST=$K_LIST in parallel"

python code/forecasting/run_forecast.py \
  --forecast_date "$DATE" \
  --method_name "$METHOD" \
  --k_list "$K_LIST" \
  --n_workers "$N_WORKERS"
