#!/bin/bash
#SBATCH -J gbqr_county
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=21
#SBATCH -t 12:00:00
#SBATCH -o logs/slurm_%j.out
#SBATCH -e logs/slurm_%j.err

set -euo pipefail

source ~/miniconda3/etc/profile.d/conda.sh
conda activate flusion

cd ~/Spatial_clustering

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

mkdir -p launcher_jobs

LAUNCHER_JOB_FILE=launcher_jobs/launcher_jobs_${METHOD}_${DATE}.txt
rm -f "$LAUNCHER_JOB_FILE"


for K in $(seq 5 25); do
  echo "python -u run_forecast.py --forecast_date $DATE --method_name $METHOD --k_min $K --k_max $K --n_workers 1" >> "$LAUNCHER_JOB_FILE"
done

module load launcher
paramrun