#!/usr/bin/Rscript

# We prepare stan_data as if we were fitting models but pass the datalist to a 
# simulation function that simulates takeup instead of passing takeup directly to 
# the MCMC sampler.

script_options <- docopt::docopt(
  stringr::str_glue("Usage:
  takeup_reduced_sbc.R sbc[--no-save   --chains=<chains> --threads=<threads> --iter=<iter> --thin=<thin> --models=<models> --outputname=<output file name> --update-output --cmdstanr --include-paths=<paths> --output-path=<path> --multilevel --test-run --save-mem --num-cores=<cores> --num-sbc-draws=<sbc> --verbose]
  
Options:
  --chains=<chains>  Number of Stan chains [default: 4]
  --threads=<threads>  Number of threads per chain [default: 1]
  --iter=<iter>  Number of (warmup + sampling) iterations [default: 8000]
  --thin=<thin>  Thin samples [default: 1]
  --include-paths=<paths>  Includes path for cmdstanr [default: .]
  --output-path=<path>  Where to save output files [default: {file.path('data', 'sbc_output_data')}]
  --num-sbc-draws=<sbc>  Number of SBC draws [default: 100]
  --num-cores=<cores>  Number of cores. NOTE: Currently not used. [default: 32]
  "),

  args = if (interactive()) "sbc --cmdstanr --num-sbc-draws=10 --iter=2000  --chains=1 --outputname=test --models=REDUCED_FORM_NO_RESTRICT --output-path=data/sbc_output_data --include-paths=stan_models --multilevel --test-run --save-mem --num-cores=2" else commandArgs(trailingOnly = TRUE)
) 

library(magrittr)
library(tidyverse)
library(parallel)
library(pbmcapply)
library(HRW)
library(splines2)
library(loo)


script_options %<>% 
  modify_at(c("chains", "iter", "threads", "num_sbc_draws", "num_cores"), as.integer) %>% 
  modify_at(c("models"), ~ c(str_split(script_options$models, ",", simplify = TRUE)))
script_options
if (script_options$cmdstanr) {
  library(cmdstanr)
} else {
  library(rstan)
  
  rstan_options(auto_write = TRUE)
}

# Defining model
if (script_options$cmdstanr == TRUE) {
  rf_model = cmdstan_model(
      "stan_models/takeup_reduced.stan")
} else {
  stop("Only cmdstanr currently implemented.")
}

chains <- as.integer(script_options$chains) # Stan chains
iter <- as.integer(script_options$iter) # Stan iterations
output_name <- if (!is_null(script_options$outputname)) { 
  script_options$outputname 
} else if (script_options$takeup) { 
  if (script_options$fit) { 
    "dist_fit" 
  } else { 
    "dist_kfold" 
  }
} else if (script_options$beliefs) {
  "beliefs"
} else if (script_options$dist) {
  "dist"
}
output_file_name <- file.path(script_options$output_path, str_c(output_name, ".csv"))
thin_by <- as.integer(script_options$thin)

source("analysis_util.R")
source(file.path("multilvlr", "multilvlr_util.R"))
source("dist_structural_util.R")
source(file.path("sbc", "dgp_functions.R"))
cat("Data\n")
# Data --------------------------------------------------------------------

load(file.path("data", "analysis.RData"))

standardize <- as_mapper(~ (.) / sd(.))
unstandardize <- function(standardized, original) standardized * sd(original)

monitored_nosms_data <- analysis.data %>% 
  filter(mon_status == "monitored", sms.treatment.2 == "sms.control") %>% 
  left_join(village.centers %>% select(cluster.id, cluster.dist.to.pot = dist.to.pot),
            by = "cluster.id") %>% 
  mutate(standard_cluster.dist.to.pot = standardize(cluster.dist.to.pot)) %>% 
  group_by(cluster.id) %>% 
  mutate(cluster_id = cur_group_id()) %>% 
  ungroup()

nosms_data <- analysis.data %>% 
  filter(sms.treatment.2 == "sms.control") %>% 
  left_join(village.centers %>% select(cluster.id, cluster.dist.to.pot = dist.to.pot),
            by = "cluster.id") %>% 
  mutate(standard_cluster.dist.to.pot = standardize(cluster.dist.to.pot)) %>% 
  group_by(cluster.id) %>% 
  mutate(cluster_id = cur_group_id()) %>% 
  ungroup()

analysis_data <- monitored_nosms_data %>% 
  mutate(assigned_treatment = assigned.treatment, assigned_dist_group = dist.pot.group)

# Splines -----------------------------------------------------------------
cat("Splines\n")

# These are used for semiparameteric models. Currently, we are not using any semiparameteric models.

num_interior_knots <- 100

cluster_standard_dist <- distinct(analysis_data, cluster_id, standard_cluster.dist.to.pot) %>% 
  arrange(cluster_id) %>% 
  pull(standard_cluster.dist.to.pot)

Z_osullivan <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, spline_type = "osullivan")
Z_i_spline <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, spline_type = "i-spline")
Z_b_spline <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, spline_type = "b-spline")

grid_dist <- get_spline_range(cluster_standard_dist) %>% unname() %>% list_modify(length = 1001) %>% do.call(seq, .)

Z_grid_osullivan <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, splines_for = grid_dist, spline_type = "osullivan")
Z_grid_i_spline <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, splines_for = grid_dist, spline_type = "i-spline")
Z_grid_b_spline <- calculate_splines(cluster_standard_dist, num_interior_knots = num_interior_knots, splines_for = grid_dist, spline_type = "b-spline")

treatment_dist <- distinct(analysis_data, assigned.treatment, standard_cluster.dist.to.pot) %>% 
  group_by(assigned.treatment) %>% 
  group_map(~ pull(.x, "standard_cluster.dist.to.pot"))

grid_dist2 <- treatment_dist %>% 
  map(get_spline_range) %>% 
  map(unname) %>% 
  map(list_modify, length = 1001) %>% 
  map(~ do.call(seq, .))

Z_grid_osullivan2 <- map2(treatment_dist, grid_dist2, ~ calculate_splines(.x, num_interior_knots = num_interior_knots, splines_for = .y, spline_type = "osullivan")) 

# Models ------------------------------------------------------------------
cat("Models\n")

num_treatments <- n_distinct(analysis_data$assigned.treatment)
num_clusters <- n_distinct(analysis_data$cluster_id)
num_counties <- n_distinct(analysis_data$county)

# Models not using "takeup_struct.stan" can be ignored

struct_model_stan_pars <- c(
  "total_error_sd", "cluster_dist_cost", "structural_cluster_benefit_cost", "structural_cluster_obs_v", "structural_cluster_takeup_prob",
  "structural_cluster_benefit", "cluster_linear_dist_cost", "cluster_quadratic_dist_cost",
  "beta", "dist_beta_v", "dist_quadratic_beta_v", "mu_rep", "cluster_cf_benefit_cost", "mu_cluster_effects_raw", "mu_cluster_effects_sd", "cluster_mu_rep", # "linear_dist_cost", 
  "cluster_rep_benefit_cost", "sim_benefit_cost",
  "group_dist_mean", "group_dist_sd", "group_dist_mix",
  "dist_beta_county_raw", "dist_beta_county_sd")

reduced_model_stan_pars <- c(
  "structural_cluster_benefit_cost", "structural_cluster_obs_v", "structural_cluster_takeup_prob",
  "beta", "cluster_cf_benefit_cost", 
  "cluster_rep_benefit_cost")

models <- lst(
  
  REDUCED_FORM_NO_RESTRICT = lst(
    model_type = 10,
    model_file = "takeup_reduced.stan",
    pars = reduced_model_stan_pars,
    control = lst(max_treedepth = 10, adapt_delta = 0.99),
    num_v_mix = 1,
    use_single_cost_model = FALSE,
    use_cost_model = cost_model_types["discrete"],
    use_private_incentive_restrictions = FALSE,
    use_salience_effect = FALSE,
    use_cluster_effects = script_options$multilevel,
    use_county_effects = script_options$multilevel,
    use_param_dist_cluster_effects = FALSE,
    use_param_dist_county_effects = FALSE,
    use_mu_cluster_effects = FALSE,
    use_mu_county_effects = FALSE,
    use_shifting_v_dist = FALSE,
    suppress_reputation = TRUE,
    
    beta_control_sd = 0.75,
    beta_far_effect_sd = 0.35,
    beta_ink_effect_sd = 0.35,
    beta_calendar_effect_sd = 0.35,
    beta_bracelet_effect_sd = 0.35,
    beta_far_ink_effect_sd = 0.25,
    beta_far_calendar_effect_sd = 0.25, 
    beta_far_bracelet_effect_sd = 0.25,
    
    reduced_beta_county_sd_sd = 0.25,
    reduced_beta_cluster_sd_sd = 0.1,
    
    iter = script_options$iter,
    thin = script_options$thin,
    sbc = FALSE # since we generate fake data in R, don't pass SBC = TRUE to stan
  )
  
)


# WTP Stan Data -----------------------------------------------------------
cat("WTP Stan Data")

wtp_stan_data <- analysis.data %>% 
  mutate(stratum = county) %>% 
  prepare_bayes_wtp_data(
    wtp.data,
    
    preference_value_diff = seq(-100, 100, 10), 
    num_preference_value_diff = length(preference_value_diff), 
    
    wtp_utility_df = 3,
    tau_mu_wtp_diff = 100,
    mu_wtp_df_student_t = 7,
    tau_sigma_wtp_diff = 50,
    sigma_wtp_df_student_t = 2.5
  )

# Treatment Details -------------------------------------------------------
treatment_formula <- ~ assigned_treatment * assigned_dist_group 

cluster_treatment_map = distinct(analysis_data, assigned_treatment, assigned_dist_group) %>% 
  arrange(assigned_dist_group, assigned_treatment) # We must arrange by distance first

treatment_map_design_matrix <- cluster_treatment_map %>%
  modelr::model_matrix(treatment_formula)

# Beliefs Data ------------------------------------------------------------

beliefs_treatment_formula <- ~ assigned_treatment 

beliefs_treatment_map_design_matrix <- cluster_treatment_map %>%
  modelr::model_matrix(beliefs_treatment_formula) %>% 
  distinct()

analysis_data %<>% 
  nest_join(
    endline.know.table.data %>% 
      filter(fct_match(know.table.type, "table.A")),
    by = "KEY.individ", 
    name = "knowledge_data"
  ) %>% 
  mutate(
    map_dfr(knowledge_data, ~ {
      tibble(
        obs_know_person = sum(.x$num.recognized),
        obs_know_person_prop = mean(.x$num.recognized),
        knows_other_dewormed = sum(fct_match(.x$dewormed, c("yes", "no")), na.rm = TRUE),
        knows_other_dewormed_yes = sum(fct_match(.x$dewormed, "yes"), na.rm = TRUE),
        thinks_other_knows = sum(fct_match(.x$second.order, c("yes", "no")), na.rm = TRUE),
        thinks_other_knows_yes = sum(fct_match(.x$second.order, "yes"), na.rm = TRUE),
      )
    }
  ))

beliefs_ate_pairs <- cluster_treatment_map %>% 
  # filter(fct_match(assigned_dist_group, "close")) %>% 
  mutate(treatment_id = seq(n())) %>% {
  bind_rows(
    left_join(., filter(., fct_match(assigned_treatment, "control")), by = c("assigned_dist_group"), suffix = c("", "_control")) %>% 
      filter(assigned_treatment != assigned_treatment_control) %>% 
      select(treatment_id, treatment_id_control),
    
    left_join(., filter(., fct_match(assigned_dist_group, "close")), by = c("assigned_treatment"), suffix = c("", "_control")) %>% 
      filter(assigned_dist_group != assigned_dist_group_control) %>% 
      select(treatment_id, treatment_id_control),
  )
} %>%
  arrange(treatment_id, treatment_id_control) 

# Stan Data ---------------------------------------------------------------

stan_data <- lst(
  # Distance Model
  use_dist_county_effects = script_options$multilevel,
  use_dist_cluster_effects = script_options$multilevel,
  
  # Beliefs Model 
  beliefs_use_stratum_level = script_options$multilevel,
  beliefs_use_cluster_level = script_options$multilevel,
  beliefs_use_obs_level = script_options$multilevel,
  
  know_table_A_sample_size = 10,
  num_beliefs_obs = filter(analysis_data, obs_know_person > 0) %>% nrow(),
  beliefs_obs_index = mutate(analysis_data, obs_index = seq(n())) %>% 
    filter(obs_know_person > 0) %>% 
    pull(obs_index),
  
  num_recognized = filter(analysis_data, obs_know_person > 0) %>% pull(obs_know_person),
  num_knows_1ord = filter(analysis_data, obs_know_person > 0) %>% pull(knows_other_dewormed),
  num_knows_2ord = filter(analysis_data, obs_know_person > 0) %>% pull(thinks_other_knows),
  
  beliefs_treatment_map_design_matrix,
  
  beliefs_ate_pairs,
  num_beliefs_ate_pairs = nrow(beliefs_ate_pairs),
  
  fit_beliefs_model_to_data = FALSE, # || script_options$takeup,
  
  # Take-up Model 
  num_obs = nrow(analysis_data),
  num_grid_obs = length(grid_dist),
  num_treatments,
  is_name_matched = !analysis_data$monitored,
  num_clusters,
  num_counties,
  obs_cluster_id = analysis_data$cluster_id,
  obs_county_id = as.integer(analysis_data$county),
  cluster_county_id = analysis_data %>% 
    distinct(cluster_id, county) %>% 
    arrange(cluster_id) %>% 
    pull(county) %>% 
    as.integer(),
  cluster_assigned_treatment = distinct(analysis_data, cluster_id, assigned.treatment) %>% 
    arrange(cluster_id) %>% 
    pull(assigned.treatment),
  takeup = analysis_data$dewormed,
  
  cluster_standard_dist = distinct(analysis_data, cluster_id, standard_cluster.dist.to.pot) %>% 
    arrange(cluster_id) %>% 
    pull(standard_cluster.dist.to.pot),
  
  cluster_treatment_map,
  
  # Rate of change
  roc_compare_treatment_id_left = cluster_treatment_map %>% 
    filter(fct_match(assigned_dist_group, "close"), fct_match(assigned_treatment, "bracelet")) %>% 
    slice(1) %>% 
    pull(assigned_treatment) %>% 
    as.integer(),
  roc_compare_treatment_id_right = cluster_treatment_map %>% 
    filter(fct_match(assigned_dist_group, "close"), fct_match(assigned_treatment, "control")) %>% 
    slice(1) %>% 
    pull(assigned_treatment) %>% 
    as.integer(),
  
  roc_distances = seq(0, 2500, 100) / sd(analysis_data$cluster.dist.to.pot),
  num_roc_distances = length(roc_distances),
  
  sim_delta_w = seq(-2, 2, 0.2),
  num_sim_delta_w = length(sim_delta_w),
  
  cluster_assigned_dist_group = distinct(analysis_data, cluster_id, dist.pot.group) %>% 
    arrange(cluster_id) %>% 
    pull(dist.pot.group),
  
  cluster_assigned_dist_group_treatment = distinct(analysis_data, cluster_id, assigned_treatment = assigned.treatment, assigned_dist_group = dist.pot.group) %>% 
    left_join(cluster_treatment_map %>% mutate(treatment_id = seq(n())), by = c("assigned_treatment", "assigned_dist_group")) %>% 
    arrange(cluster_id) %>% 
    pull(treatment_id),
  
  num_dist_group_treatments = n_distinct(cluster_assigned_dist_group_treatment),
  grid_dist,
  grid_dist2,
  
  num_discrete_dist = 2,
  
  num_dist_group_mix = script_options$num_mix_groups,
  
  small_grid_dist = grid_dist[(seq_along(grid_dist) %% 100) == 0],
  num_small_grid_obs = length(small_grid_dist),
  
  Z_splines_v = Z_osullivan,
  num_knots_v = ncol(Z_splines_v),
  
  Z_grid_v = Z_grid_osullivan,
  Z_grid_v2 = Z_grid_osullivan2,
  
 
  num_excluded_clusters = 0,
  excluded_clusters = array(dim = 0),

  use_name_matched_obs = FALSE, 
  is_structural = FALSE, 
  use_binomial = FALSE,
  use_single_cost_model = FALSE,
  use_cost_k_restrictions = TRUE,
  use_shifting_v_dist = FALSE,
  use_u_in_delta = FALSE,
  multithreaded = script_options$threads > 1,
  cluster_log_lik = TRUE,
  generate_rep = FALSE,
  generate_sim = FALSE,
  fit_model_to_data = TRUE,
  fit_wtp_model_to_data = FALSE,
  fit_dist_model_to_data = FALSE,
  cross_validate = FALSE,
  use_wtp_model = FALSE,
  use_homoskedastic_shocks = FALSE,
  lognormal_dist_model = TRUE,
  
  thin = thin_by,
  
  alg_sol_f_tol = 1e-5,
  alg_sol_rel_tol = 1e-5, 
  alg_sol_max_steps = 1e6L,
  
  CALENDAR_TREATMENT_INDEX = which(fct_match(fct_unique(cluster_assigned_treatment), "calendar")),
  BRACELET_TREATMENT_INDEX = which(fct_match(fct_unique(cluster_assigned_treatment), "bracelet")),
  
  # Priors
  
  u_splines_v_sigma_sd = 1,
  
  dist_beta_v_sd = 0.25,
  
  structural_beta_county_sd_sd = 0.25,
  structural_beta_cluster_sd_sd = 0.25,
 
  hyper_dist_mean_mean = 0.75, 
  hyper_dist_mean_sd = 0.5,
  hyper_dist_sd_sd = 0.25,
  
  county_dist_effect_sd_sd = 0.1, 
  cluster_dist_effect_sd_sd = 0.075, 
  
  analysis_data
) %>% 
  list_modify(!!!map(models, pluck, "model_type") %>% set_names(~ str_c("MODEL_TYPE_", .))) %>% 
  list_modify(!!!wtp_stan_data) 


default_gen_data_args = c(
    models$REDUCED_FORM_NO_RESTRICT,
    stan_data
)
default_gen_data_args = c(stan_data, list(dist_mean = 3), default_gen_data_args, models$REDUCED_FORM_NO_RESTRICT)
default_sample_from_model_args = list(
  stan_model = rf_model, 
  cmdstanr = script_options$cmdstanr,
  iter = script_options$iter,
  chains = script_options$chains,
  verbose = script_options$verbose)

if (script_options$test_run == TRUE) {
  gen_data = meta_gen_data(default_gen_data_args)
  fake_data = gen_data(1)
  gen_params = meta_gen_params()
  sample_from_model = meta_sample_from_model(default_sample_from_model_args)

  fake_params = gen_params(1, fake_data)
  gen_modelled_data = meta_gen_modelled_data()
  fake_mod = gen_modelled_data(1, fake_data, fake_params)


  sim_draw = simulate_draw(
      seed = 1,
      sim_args = list(fit_model = TRUE),
      gen_data_args = default_gen_data_args,
      gen_params_args = NULL,
      gen_modelled_data_args = NULL,
      sample_from_model_args = default_sample_from_model_args
  )
}



create_sbc_output = function(stan_fit, params, save_mem = TRUE){
    tidy_draws = stan_fit %>%
        gather_draws(`beta_.*`[j], regex = TRUE)
    true_param_df = tibble(
        true_value = unlist(params),
        param_name = names(unlist(params))
    ) %>%
        mutate(j = str_extract(param_name, "\\d+$") %>% as.numeric,
            term = str_remove(param_name, "\\d+$")) %>%
        select(term, j, true_value)

    subset_true_param_df = true_param_df %>%
        filter(str_detect(term, "control|ink|calendar|bracelet"))

    comp_df = inner_join(
        tidy_draws,
        subset_true_param_df,
        by = c(".variable" = "term", "j")
    )

    if (save_mem == FALSE){
      return(comp_df)
    }
  
    rank_stat_df = comp_df %>%
        group_by(.variable, j) %>%
        summarise(
            rank_stat = mean(.value < true_value), .groups = "drop"
        )
    return(rank_stat_df)
}


library(furrr)
library(tidybayes)

# fixing this bug:
# https://github.com/HenrikBengtsson/future/issues/609
## May 'datatable.*' R options created by the data.table package stick
dummy <- value(lapply(seq_len(nbrOfWorkers()), FUN = function(ii) {
  future(NULL, packages = "data.table", earlySignal = TRUE)
}))


cat("Setting up plan\n")
if (interactive()) {
  plan(multisession, workers = script_options$num_cores)
} else {

  plan(cluster, workers = availableWorkers())
}

simulated_draws = 1:script_options$num_sbc_draws %>%
  future_map(
    ~{sim_draw = simulate_draw(
      seed = .x,
      gen_data_args = default_gen_data_args,
      gen_params_args = NULL,
      gen_modelled_data_args = NULL,
      sample_from_model_args = default_sample_from_model_args,
      sim_args = list(fit_model = TRUE)
      )
      
      
      sbc_draw = create_sbc_output(
          sim_draw$bayes_fit,
          sim_draw$params,
          save_mem = script_options$save_mem
      )
      sbc_draw$draw = .x 

        return(sbc_draw)
        },
    .options = furrr_options(seed = TRUE, packages = c("tidybayes"))
  )

cat("Saving Data\n")
simulated_draw_df = bind_rows(simulated_draws)

write_csv(
  simulated_draw_df,
  output_file_name
)




