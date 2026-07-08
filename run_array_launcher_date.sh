#!/bin/bash
#SBATCH -J gbqr_dates
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=21
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

mkdir -p launcher_jobs logs

METHOD="${METHOD:?METHOD is required}"
DATE_LIST="${DATE_LIST:?DATE_LIST is required}"
K_LIST="${K_LIST:-7 9 15 21 23 31 45 61}"
K_VALUES=$(printf '%s' "$K_LIST" | tr ',' ' ')

export LAUNCHER_JOB_FILE=launcher_jobs/launcher_jobs_${METHOD}_${SLURM_JOB_ID}.txt
rm -f "$LAUNCHER_JOB_FILE"

for DATE in $DATE_LIST; do
  for K in $K_VALUES; do
    echo "python -u code/forecasting/run_forecast.py --forecast_date $DATE --method_name $METHOD --k_list $K --n_workers 1" >> "$LAUNCHER_JOB_FILE"
  done
done

echo "METHOD=$METHOD"
echo "K_LIST=$K_LIST"
echo "K_VALUES=$K_VALUES"
echo "DATE_LIST=$DATE_LIST"
echo "LAUNCHER_JOB_FILE=$LAUNCHER_JOB_FILE"
echo "Number of commands: $(wc -l < "$LAUNCHER_JOB_FILE")"

cat "$LAUNCHER_JOB_FILE"

${LAUNCHER_DIR}/paramrun
