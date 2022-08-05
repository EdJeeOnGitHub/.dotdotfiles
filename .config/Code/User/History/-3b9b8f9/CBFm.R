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

################# Simulations ##################################################


# Generate param grid we wanna use for first figure
# TODO: Put this into script_options so can just pass to bash.
param_grid = expand.grid(
    N = c(400, 600, 800, 1000, 1200, 1400),
    rho = c(0.1, 0.25, 0.5, 0.75, 0.9)
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
        param_grid$n_clusters
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
            baseline_mean = 0.1
        ),
        batch = N_per_round,
        sim_options = script_options
    ),
    .options = furrr_options(
        seed = TRUE
    ),
    .progress = TRUE
)
tictoc::toc()
# Save using timestamp - can always recover hyper params from csv so no need 
# for descriptive name.
# BUT would be good to save a single row param file with same timestamp in future.
sim_result_file_name = paste0("sim-results-", format(now, "%Y%m%d%H%M"),  ".csv")
sim_df %>%
    write_csv(paste0("data/output/", sim_result_file_name))


# same again but for figure 2
tictoc::tic()
second_param_grid = expand.grid(
    N = seq(from = 400, to = 2000, by = 200), 
    rho = c(0.1, 0.25, 0.5, 0.75, 0.9)
) %>% as_tibble() %>%
    mutate(n_clusters = floor(N/8)) 
second_sim_df = future_pmap_dfr(
    list(
        second_param_grid$N,
        second_param_grid$rho,
        second_param_grid$n_clusters
    ),
    ~run_sim(
        N = N_rounds,
        hyper_param_func = function(TEs) default_hyper_params(
            TEs = TEs,
            N = ..1,
            rho = ..2,
            n_clusters = ..3,
            dgp = script_options$dgp,
            baseline_mean = 0.1, 
            pr_arm = c(1/3, 2/3)
        ),
        batch = N_per_round, 
        sim_options = script_options
    ),
    .options = furrr_options(
        seed = TRUE
    ),
    .progress = TRUE
)
tictoc::toc()




second_sim_result_file_name = paste0("second-sim-results-", format(now, "%Y%m%d%H%M"),  ".csv")
second_sim_df %>%
    write_csv(paste0("data/output/", second_sim_result_file_name))



# server clean up
if (!interactive()) {
  stopCluster(cl)
  mpi.quit()
}


######### Old scratch Stuff ####################################################
# second_sim_df %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(draw = 1:n()) %>%
#     # filter(draw > 1000) %>%
#     summarise( 
#         mean_TE = mean(TE), 
#         sd = sd(TE),
#         median_TE = median(TE)
#     ) %>%
#     ggplot(aes( 
#         x = N, 
#         colour = factor(rho),
#         y = mean_TE,
#         ymin = mean_TE - 1.96*sd,
#         ymax = mean_TE + 1.96*sd
#     )) +
#     geom_pointrange() +
#     geom_line() +
#     theme_bw()

# second_sim_df %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(draw = 1:n()) %>%
#     filter(draw > 200) %>%
#     summarise( 
#         mean_TE = mean(TE), 
#         sd = sd(TE),
#         median_TE = median(TE)
#     ) %>%
#     ggplot(aes( 
#         x = N, 
#         colour = factor(rho),
#         y = mean_TE
#     )) +
#     geom_point() +
#     geom_line() +
#     theme_bw()

# second_sim_df %>%
#     filter(rho == 0.1) %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(draw = 1:n()) %>%
#     filter(draw > 200) %>%
#     summarise( 
#         mean_TE = mean(TE), 
#         sd = sd(TE),
#         median_TE = median(TE)
#     ) 

# second_sim_df %>%
#     filter(rho == 0.1) %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(draw = 1:n()) %>%
#     filter(draw > 100) %>%
#     ggplot(aes( 
#         x = draw, 
#         y = TE, 
#         colour = factor(N)
#     )) +
#     geom_point(alpha = 0.4) +
#     theme_minimal() +
#     labs(title = "Power Bandit")

# sim_df %>%
#     filter(rho == 0.1) %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(draw = 1:n()) %>%
#     filter(draw > 100) %>%
#     ggplot(aes( 
#         x = draw, 
#         y = TE, 
#         colour = factor(N)
#     )) +
#     geom_point(alpha = 0.4) +
#     theme_minimal() +
#     labs(title = "Power Bandit")

# sim_df %>%
#     group_by(N, rho, model, vcv) %>%
#     mutate(
#         bin = cut_width(TE, 0.02), 
#         signif = as.numeric(signif)
#     ) %>%
#     group_by(N, rho, bin) %>%
#     summarise( 
#         n_bin = n(), 
#         min_TE = min(TE), 
#         mean_TE = mean(TE),
#         pr_signif = mean(signif)
#     ) %>%
#     mutate(dist_80 = abs(pr_signif - 0.8)) %>%
#     filter(dist_80 == min(dist_80) & dist_80 < 0.05) %>%
#     summarise_all(mean) %>%
#     select(
#         N, rho, bin, pr_signif,min_TE, mean_TE, n_bin)  %>%
#     ggplot(aes( 
#         x = N, 
#         y = min_TE, 
#         colour = factor(rho)
#     )) +
#     geom_point() +
#     geom_line()
    
    
#      %>%
#     ggplot(aes( 
#         x = N, 
#         y = min_TE, 
#         colour = factor(rho)
#     )) +
#     geom_point() +
#     geom_line()



# tictoc::tic()
# initial_sim = run_sim(
#     N = 50,
#     hyper_param_func = function(TEs) default_hyper_params(
#         TEs = TEs, 
#         dgp = "icc",
#         rho = 0.1,
#         baseline_mean = 0.1,
#         N = 400, n_clusters = 400/8)
# ) 
# tictoc::toc()
# initial_sim

# initial_sim %>%
#     mutate(x = 1:n()) %>%
#     filter( x > 25) %>%
#     summarise(mean(signif))

# initial_sim %>%
#     mutate(x = 1:n()) %>%
#     filter(vcv == "standard", model == "chisq") %>%
#     ggplot(aes(
#         x = x, 
#         y = TE,
#         colour = vcv
#     )) +
#     geom_point() +
#     theme_minimal() +
#     labs( 
#         x = "Iteration", 
#         title = "MDE Bandit Kind Of"
#     ) +
#     geom_smooth()   


# initial_sim %>%
#     mutate(signif = as.numeric(signif)) %>%
#     filter(
#         model == "chisq" & vcv == "standard"
#     ) %>%
#     mutate(bin = cut_width(TE, 0.01)) %>%
#     group_by( 
#         bin
#     ) %>%
#     summarise(across(where(is.numeric), mean)) %>%
#     ggplot(aes(x = TE, y = signif)) +
#     geom_point() + 
#     geom_line() +
#     geom_hline(yintercept = 0.8)

# initial_sim %>%
#     group_by(model, vcv) %>%
#     mutate(bin = cut_width(TE, 0.005)) %>%
#     group_by(TE_bin = bin, model, vcv) %>%
#     summarise(pr_signif = mean(signif), 
#               n_draws = n())  %>%
#     filter( 
#         pr_signif > 0.8 & pr_signif < 0.9
#     )

# ed %>%
#     mutate(bin = cut_width(TE, 0.01)) %>%
#     group_by(TE_bin = bin) %>%
#     summarise(pr_signif = mean(signif), 
#               n_draws = n()) 

# ed %>%
#     mutate(bin = cut_number(TE, nrow(.)/30)) %>%
#     group_by(bin) %>%
#     summarise(pr_signif = mean(signif), 
#               bin_mean = mean(TE))  %>%
#     ungroup() %>%
#     ggplot(aes( 
#         x = bin_mean, 
#         y = pr_signif
#     )) +
#     geom_point() 






# ed %>%
#   ggplot(aes(
#       x = TE, 
#       y = as.numeric(signif)
#   )) +
#   stat_summary_bin(fun.y='mean', bins = 50,
#                    color='orange', size=2, geom='point') +
#                    geom_hline(yintercept = 0.8)