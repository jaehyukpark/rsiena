#!/bin/bash
#SBATCH --mem=20GB
#SBATCH --exclusive
#SBATCH -n 1
#SBATCH -N 2
#SBATCH -c 12
#SBATCH -p parallel

module load R/4.3.1

Rscript rsiena_artist_network.R 1

