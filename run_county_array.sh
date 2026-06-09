#!/bin/bash
#SBATCH -J gbqr_county
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=21
#SBATCH -t 04:00:00
#SBATCH -o logs/slurm_%j.out
#SBATCH -e logs/slurm_%j.err

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

source ~/miniconda3/etc/profile.d/conda.sh
conda activate flusion

cd ~/Spatial_clustering

echo "Running METHOD=$METHOD, DATE=$DATE, k=5~25 in parallel"

python run_county_forecast.py \
  --forecast_date "$DATE" \
  --method_name "$METHOD" \
  --k_min 5 \
  --k_max 25 \
  --n_workers 21