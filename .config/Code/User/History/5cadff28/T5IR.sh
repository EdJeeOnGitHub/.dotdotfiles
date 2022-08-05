#!/usr/bin/env bash

#SBATCH --partition=broadwl
#SBATCH --job-name=takeup        # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=24       # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem-per-cpu=1G         # memory per cpu-core (4G is default)
#SBATCH --time=0-06:00:00        # maximum time needed (HH:MM:SS)
#SBATCH --mail-type=begin        # send email when job begins
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-user=karimn2.0@gmail.com
#SBATCH --output=temp/log/takeup-%j.log
#SBATCH --error=temp/log/takeup-%j.log
#SBATCH --export=IN_SLURM=1

LATEST_VERSION=53
VERSION=${1:-$LATEST_VERSION} # Get version from command line if provided
CMDSTAN_ARGS="--cmdstanr"
SLURM_INOUT_DIR=~/scratch-midway2

if [[ -v IN_SLURM ]]; then
  echo "Running in SLURM..."

  module load midway2 gdal/2.4.1 udunits cmake R/4.2.0

  OUTPUT_ARGS="--output-path=${SLURM_INOUT_DIR}"
  POSTPROCESS_INOUT_ARGS="--input-path=${SLURM_INOUT_DIR} --output-path=${SLURM_INOUT_DIR}"
  CORES=$SLURM_CPUS_PER_TASK

  echo "Running with ${CORES} cores."
  echo "INOUT ARGS: ${POSTPROCESS_INOUT_ARGS}."
else
  OUTPUT_ARGS="--output-path=data/stan_analysis_data"
  POSTPROCESS_INOUT_ARGS=
  CORES=12
fi

STAN_THREADS=$((${CORES} / 4))

# WTP 
# Rscript ./run_takeup.R wtp prior --chains=4 --iter 2000 --outputname=wtp_model_prior --output-path=data/stan_analysis_data --multilevel &
# Rscript ./run_takeup.R wtp fit   --chains=4 --iter 2000 --outputname=wtp_model_fit   --output-path=data/stan_analysis_data --multilevel
# wait

# Distance
#Rscript ./run_takeup.R dist prior --chains=4 --iter 800 --outputname=dist_model_prior --output-path=data/stan_analysis_data --num-mix-groups=1 --multilevel &
#Rscript ./run_takeup.R dist fit   --chains=4 --iter 800 --outputname=dist_model_fit   --output-path=data/stan_analysis_data --num-mix-groups=1 --multilevel
#wait

# Beliefs
#Rscript ./run_takeup.R beliefs prior --chains=4 --iter 1000 --outputname=beliefs_prior --output-path=data/stan_analysis_data &
#Rscript ./run_takeup.R beliefs fit   --chains=4 --iter 1000 --outputname=beliefs       --output-path=data/stan_analysis_data --num-mix-groups=1
#wait

# Reduced Form 
# Rscript ./run_takeup.R takeup prior --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_prior${VERSION} --multilevel --age --sequential
# Rscript ./run_takeup.R takeup fit   --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --multilevel --age --sequential
# Rscript ./run_takeup.R takeup cv    --models=REDUCED_FORM_NO_RESTRICT ${CMDSTAN_ARGS} ${OUTPUT_ARGS}             --update --outputname=dist_kfold${VERSION} --folds=10

# Structural

# Rscript ./run_takeup.R takeup prior --models=STRUCTURAL_LINEAR_U_SHOCKS                     ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_prior${VERSION} --num-mix-groups=1 --sequential
# Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS                     ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
# Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS_NO_SUBMODELS        ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
#Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS_NO_BELIEFS_SUBMODEL  ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
#Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS_NO_WTP_SUBMODEL      ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
#Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS_NO_TAKEUP            ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
Rscript ./run_takeup.R takeup fit   --models=STRUCTURAL_LINEAR_U_SHOCKS_NO_WTP_TAKEUP          ${CMDSTAN_ARGS} ${OUTPUT_ARGS} --threads=${STAN_THREADS} --update --outputname=dist_fit${VERSION}   --num-mix-groups=1 --sequential
# Rscript ./run_takeup.R takeup cv    --models=STRUCTURAL_LINEAR_U_SHOCKS ${CMDSTAN_ARGS} ${OUTPUT_ARGS}             --update --outputname=dist_kfold${VERSION} --num-mix-groups=1 --folds=10

Rscript ./postprocess_dist_fit.R ${VERSION} ${POSTPROCESS_INOUT_ARGS} --cores=1 --load-from-csv

# Simulation
# ./run_stan_dist_sim.R ${CMDSTAN_ARGS} 



Rscript ./run_takeup.R sbc struct \
  --num-mix-groups=1 \
  --chains=1 \
  --cores=1 \
  --num-sbc-sims=200 \
  --models=STRUCTURAL_LINEAR_U_SHOCKS \
  --output-path=data/sbc_output_data \ 
  --include-paths=stan_models \
  --iter=2000 \
  --outputname=initial_test \
  --multilevel 