#!/bin/bash
#SBATCH --job-name=lobster
#SBATCH --array=1-100
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=115GB
#SBATCH --output=logs/bma-%A_%a.out
#SBATCH --error=logs/bma-%A_%a.err

cd market-responses-to-a-vix-impulse

Rscript 01-lobster-extract-data.R