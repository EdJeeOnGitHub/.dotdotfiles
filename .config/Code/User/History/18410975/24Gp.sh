#!/usr/bin/env bash

#SBATCH --partition=broadwl
#SBATCH --job-name=sbc-takeup        # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=4       # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=2G        # memory per cpu-core (4G is default)
#SBATCH --time=0-06:00:00        # maximum time needed (HH:MM:SS)
#SBATCH --mail-type=begin        # send email when job begins
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-user=edjee96@gmail.com
#SBATCH --output=temp/log/takeup_sbc-%j.log
#SBATCH --error=temp/log/takeup_sbc-%j.log
#SBATCH --export=IN_SLURM=1

LATEST_VERSION=53
VERSION=${1:-$LATEST_VERSION} # Get version from command line if provided
CMDSTAN_ARGS="--cmdstanr --include-paths=takeup/stan_models"

if [[ -v IN_SLURM ]]; then
  echo "Running in SLURM..."

  module load midway2 gdal/2.4.1 udunits cmake R/4.2.0 openmpi

  CORES=$SLURM_CPUS_PER_TASK

  echo "Running with ${CORES} cores."
else
  CORES=12
fi

STAN_THREADS=1


# SBC
Rscript ./takeup_reduced_sbc.R sbc --cmdstanr \
                                   --num-sbc-draws=10 \
                                   --iter=2000 \
                                   --chains=1 \
                                   --outputname=test \
                                   --models=REDUCED_FORM_NO_RESTRICT \
                                   --output-path=data/sbc_output_data \
                                   --include-paths=stan_models \
                                   --multilevel \
                                   --test-run \
                                   --save-mem \
                                   --num-cores=2
