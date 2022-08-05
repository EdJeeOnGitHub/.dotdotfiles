script_options <- docopt::docopt(sprintf(
"Usage:
  run_sim [options]
 Options:
  --num-rounds=<num-rounds>, -r <num-rounds>  Number of rounds in the trial [default: 15]
  --num-per-round=<num-per-round>, -n <num-per-round>  Number of participants per round [default: 100]
  --num-cluster-cores=<num-cluster-cores>  Number of cores to use in 'outer' cluster [default: 32]
  --p-adjust-method=<p-adjust-method>, -p <p-adjust-method> Method to adjust p-values for multiple testing [default: none]
  --num-p-adjust=<num-p-adjust>, -P <num-p-adjust>  Number of comparisons to adjust for [default: 1]
  --dgp=<dgp>, -d <dgp>  Data generating process to use [default: icc]
"),
  args = if (interactive()) "--num-cluster-cores=4" else commandArgs(trailingOnly = TRUE) 
)

source("code/power-functions.R")
source("code/sims.R")
library(furrr)
now = Sys.time()
script_options = script_options %>%
    modify_at(.at = vars(
      contains("num")), as.integer) 

N_rounds = script_options$num_rounds
N_per_round = script_options$num_per_round

script_options


# Generate param grid we wanna use for first figure
# TODO: Put this into script_options so can just pass to bash.
param_grid = expand.grid(
    N = 1660,
    rho = c(0.1, 0.2),
    randomisation_level = c("individual", "cluster"),
    spillover_pct = c(0.1, 0.2)
) %>% as_tibble() %>%
    mutate(n_clusters = floor(N/8)) 

tictoc::tic()
# If running locally just use 8 cores
if (interactive()) {
    plan(multisession, workers = 8)
} else { # if called externally presume we're in a server enviroment
  library(parallel)
  library(Rmpi)
  # Generate clusters specified from bash
  cl = makeCluster(script_options$num_cluster_cores, type = "MPI")
  plan(
    list(
      tweak(future::cluster, workers = cl),
      multicore
    )
  )
}


# map over grid options
sim_df = future_pmap_dfr(
    list(
        param_grid$N,
        param_grid$rho,
        param_grid$n_clusters,
        param_grid$randomisation_level,
        param_grid$spillover_pct
    ),
    ~run_sim(
        N = N_rounds,
        # anon func for TEs
        hyper_param_func = function(TEs) default_hyper_params(
            TEs = TEs,
            N = ..1,
            rho = ..2,
            n_clusters = ..3,
            dgp = script_options$dgp,
            baseline_mean = 0.1,
            randomisation_level = ..4,
            n_villages = 100,
            spillover_pct = ..5
        ),
        batch = N_per_round,
        sim_options = script_options
    ),
    .options = furrr_options(
        seed = TRUE
    ),
    .progress = TRUE
)
stop()
tictoc::toc()

# Save using timestamp - can always recover hyper params from csv so no need 
# for descriptive name.
# BUT would be good to save a single row param file with same timestamp in future.
sim_result_file_name = paste0("spillover-sim-results-", format(now, "%Y%m%d%H%M"),  ".csv")
sim_df %>%
    write_csv(paste0("data/output/", sim_result_file_name))
