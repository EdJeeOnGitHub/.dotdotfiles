source("sbc/dgp-functions.R")

default_gen_data_args = lst(
    num_obs = 100,
    num_clusters = 10,
    num_counties = 4,
    num_treatments = 4,
    num_discrete_dist = 2,
    
    use_cluster_effects = 1,
    use_county_effects = 1
    use_binomial = 1,
    use_cost_k_restrictions = 0,
    use_private_incentive_restrictions = 0
    use_name_matched_obs = 0,
    use_shifting_v_dist = 0,
    multithreaded = 0,
    generate_rep = 0,
    generate_sim = 0,
    cross_validate = 0,
    alg_sol_f_tol = 1e-10,
    alg_sol_rel_tol = 1e-3,
    alg_sol_ma

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
gen_data = meta_gen_data(

)