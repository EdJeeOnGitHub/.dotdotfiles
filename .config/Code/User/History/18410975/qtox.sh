#!/usr/bin/env bash

#SBATCH --partition=broadwl
#SBATCH --job-name=sbc-takeup        # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=24       # cpu-cores per task (>1 if multi-threaded tasks)
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
SLURM_INOUT_DIR=~/scratch-midway2

if [[ -v IN_SLURM ]]; then
  echo "Running in SLURM..."

  module load midway2 gdal/2.4.1 udunits cmake R/4.2.0 openmpi

  OUTPUT_ARGS="--output-path=${SLURM_INOUT_DIR}"
  POSTPROCESS_INOUT_ARGS="--input-path=${SLURM_INOUT_DIR} --output-path=${SLURM_INOUT_DIR}"
  CORES=$SLURM_CPUS_PER_TASK

  echo "Running with ${CORES} cores."
  echo "INOUT ARGS: ${POSTPROCESS_INOUT_ARGS}."
else
  OUTPUT_ARGS="--output-path=data/sbc_output_data"
  POSTPROCESS_INOUT_ARGS=
  CORES=12
fi

STAN_THREADS=1


# SBC
Rscript ./takeup_reduced_sbc.R sbc --models=REDUCED_FORM_NO_RESTRICT \
    ${CMDSTAN_ARGS} \
    --output-path="data/sbc_output_data" \
    --threads = 1 \
    --num-cores = $SLURM_CPUS_PER_TASK \
    --save-mem \
    --iter=2000 \
    --chains=1 \
    --num-sbc-draws=500 \
    --outputname=initial_reduced_form_sbc${VERSION} \
    --multilevel

# Distance
#Rscript ./run_takeup.R dist prior --chains=4 --iter 800 --outputname=dist_model_prior --output-path=~/Code/takeup/data/stan_analysis_data --include-paths=~/Code/takeup/stan_models --num-mix-groups=1 --multilevel &
#Rscript ./run_takeup.R dist fit   --chains=4 --iter 800 --outputname=dist_model_fit   --output-path=~/Code/takeup/data/stan_analysis_data --include-paths=~/Code/takeup/stan_models --num-mix-groups=1 --multilevel
#wait

# Beliefs
#Rscript ./run_takeup.R beliefs prior --chains=4 --iter 1000 --outputname=beliefs_prior --output-path=~/Code/takeup/data/stan_analysis_data --include-paths=~/Code/takeup/stan_models &
#Rscript ./run_takeup.R beliefs fit   --chains=4 --iter 1000 --outputname=beliefs       --output-path=~/Code/takeup/data/stan_analysis_data --include-paths=~/Code/takeup/stan_models --num-mix-groups=1
#wait

# Reduced Form 
#Rscript ./run_takeup.R takeup prior --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_prior${VERSION} --multilevel --age --sequentia
#Rscript ./run_takeup.R takeup fit   --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --multilevel --age --sequential
# Rscript ./run_takeup.R takeup cv    --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS}             --update --outputname=dist_kfold${VERSION} --folds=10

# Structural

# Rscript ./run_takeup.R takeup prior --models=STRUCTURAL_LINEAR_U_SHOCKS ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_prior${VERSION} --num-mix-groups=1 --sequential
# Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
# Rscript ./run_takeup.R takeup cv    --models=STRUCTURAL_LINEAR_U_SHOCKS ${CMDSTAN_ARGS} ${OUTPUT_ARGS}             --update --outputname=dist_kfold${VERSION} --num-mix-groups=1 --folds=10

# Rscript ./postprocess_dist_fit.R ${VERSION} ${POSTPROCESS_INOUT_ARGS} --cores=1 --load-from-csv

# Simulation
# ./run_stan_dist_sim.R ${CMDSTAN_ARGS} 
