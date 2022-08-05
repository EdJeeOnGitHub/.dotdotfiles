source("sbc/dgp-functions.R")
library(cmdstanr)
default_gen_data_args = lst(
    num_obs = 1000,
    num_clusters = 100,
    num_counties = 4,
    num_treatments = 4,
    num_discrete_dist = 2,
    
    multithreaded = 0,
    generate_rep = 0,
    generate_sim = 0,
    cross_validate = 0,
    alg_sol_f_tol = 1e-10,
    alg_sol_rel_tol = 1e-3,
    alg_sol_max_steps = 100,

    dist_mean = 3

)

REDUCED_FORM_NO_RESTRICT = lst(
    use_single_cost_model = FALSE,
    use_private_incentive_restrictions = FALSE,
    use_salience_effect = FALSE,
    use_cluster_effects = 1,
    use_county_effects = 1,
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
    reduced_beta_cluster_sd_sd = 0.1

)

default_gen_data_rf_args = c(default_gen_data_args, REDUCED_FORM_NO_RESTRICT)
default_gen_data_rf_args
gen_data = meta_gen_data(default_gen_data_rf_args)
fake_data = gen_data(1)

gen_params = meta_gen_params()

fake_params = gen_params(1, fake_data)
gen_modelled_data = meta_gen_modelled_data()
fake_mod = gen_modelled_data(1, fake_data, fake_params)

rf_model = cmdstan_model("stan_models/takeup_reduced.stan")

simulate_draw(
    seed = 1,
    sim_args = list(fit_model = TRUE),
    gen_data_args = default_gen_data_args,
    gen_params_args = NULL,
    gen_modelled_data_args = NULL,
    stan_model = 
)


