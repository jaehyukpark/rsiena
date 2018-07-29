#!/bin/bash

#SBATCH -t 24:00:00
#SBATCH --mem=20GB
#SBATCH --exclusive
#SBATCH -n 1
#SBATCH -c 24
#SBATCH -p serial

module load R/4.3.1

Rscript rsiena_artist_network.R 1

