#!/bin/bash
#SBATCH --job-name=bma
#SBATCH --array=1-600
#SBATCH --cpus-per-task=1
#SBATCH --output=logs/bma-%A_%a.out
#SBATCH --error=logs/bma-%A_%a.err

cd bayesian-nse

Rscript 03_evaluate_bayesian_models.R