#!/bin/bash
#SBATCH -J gbqr_all
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=10
#SBATCH --cpus-per-task=1
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

module load launcher/3.9

mkdir -p launcher_jobs

export LAUNCHER_JOB_FILE=launcher_jobs/launcher_jobs_${METHOD}_${DATE}.txt
rm -f "$LAUNCHER_JOB_FILE"

<<<<<<< HEAD
for K in $(seq 47 2 65); do
  echo "python -u code/forecasting/run_forecast.py --forecast_date $DATE --method_name $METHOD --k_min $K --k_max $K --n_workers 1" >> "$LAUNCHER_JOB_FILE"
done

#for K in $(seq 2 22); do
#  echo "python -u code/forecasting/run_hsa_forecast.py --forecast_date $DATE --method_name $METHOD --k_min $K --k_max $K --n_workers 1" >> "$LAUNCHER_JOB_FILE"
#done

=======
K_LIST="${K_LIST:-7,9,15,21,23,31,45,61}"
IFS=',' read -ra K_VALUES <<< "$K_LIST"

for K in "${K_VALUES[@]}"; do
  echo "python -u code/forecasting/run_forecast.py --forecast_date $DATE --method_name $METHOD --k_list $K --n_workers 1" >> "$LAUNCHER_JOB_FILE"
done

echo "K_LIST=$K_LIST"
>>>>>>> 3e953b4b186525b16c9847e27d99c09d6d97d43a
echo "LAUNCHER_JOB_FILE=$LAUNCHER_JOB_FILE"
echo "LAUNCHER_DIR=$LAUNCHER_DIR"

${LAUNCHER_DIR}/paramrun
