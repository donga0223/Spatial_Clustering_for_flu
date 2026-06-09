#!/bin/bash
#SBATCH -J gbqr_hsa
#SBATCH -p small
#SBATCH -N 1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=19
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

echo "Running METHOD=$METHOD, DATE=$DATE, k=2~20 in parallel"
python run_forecast.py \
--forecast_date "$DATE" \
--method_name "$METHOD" \
--k_min 2 \
--k_max 20 \
--n_workers 19