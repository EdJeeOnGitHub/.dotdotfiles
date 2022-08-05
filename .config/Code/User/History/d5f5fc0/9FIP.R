# Create DGP functions for simulation based calibration


meta_gen_data = function(gen_data_args) {
    function(seed) {
        set.seed(seed + 189)

        ## base_data_sec.stan section -----------------------------------------------
        num_obs = gen_data_args$num_obs
        num_clusters = gen_data_args$num_clusters
        num_counties = gen_data_args$num_counties
        num_treatments = gen_data_args$num_treatments
        num_discrete_dist = gen_data_args$num_discrete_dist
        # TODO add ages
        num_age_groups = 1
        obs_age_group = rep(1, num_obs)
        use_age_group_gp = FALSE
        age_group_alpha_sd = array(0, dim = 8)
        age_group_rho_sd = array(0, dim = 8)

        # Note implies clusters approx equal size - no skewing here.
        obs_cluster_id = sample(1:num_clusters, size = num_obs, replace = TRUE)
        cluster_county_id = sample(1:num_counties, size = num_clusters, replace = TRUE)
        obs_county_id = cluster_county_id[obs_cluster_id]

        cluster_treatment_map = matrix(
            NA,
            ncol = 2,
            nrow = num_treatments*num_discrete_dist
            )
        cluster_treatment_map[, 1] = rep(1:num_treatments, num_discrete_dist)
        cluster_treatment_map[, 2] = rep(1:num_discrete_dist, each = num_treatments)

        cluster_assigned_dist_group_treatment = sample(
            1:(num_treatments*num_discrete_dist),
            size = num_clusters,
            replace = TRUE)

        cluster_standard_dist = rnorm(
            n = num_clusters,
            mean = gen_data_args$dist_mean,
            sd = 1
        )
        ## takeup_data_sec.stan section ----------------------------------------
        use_cluster_effects = gen_data_args$use_cluster_effects
        use_county_effects = gen_data_args$use_county_effects
        use_binomial = gen_data_args$use_binomial
        use_cost_k_restrictions = gen_data_args$use_cost_k_restrictions
        use_private_incentive_restrictions = gen_data_args$use_private_incentive_restrictions
        use_salience_effect = gen_data_args$use_salience_effect
        use_single_cost_model = gen_data_args$use_single_cost_model
        use_name_matched_obs = gen_data_args$use_name_matched_obs
        use_shifting_v_dist = gen_data_args$use_shifting_v_dist 
        multithreaded =  gen_data_args$multithreaded
        generate_rep = gen_data_args$generate_rep
        generate_sim = gen_data_args$generate_sim
        fit_model_to_data = 1
        cross_validate = gen_data_args$cross_validate

        alg_sol_f_tol = gen_data_args$alg_sol_f_tol
        alg_sol_rel_tol = gen_data_args$alg_sol_rel_tol
        alg_sol_max_steps = gen_data_args$alg_sol_max_steps

        CALENDAR_TREATMENT_INDEX = gen_data_args$CALENDAR_TREATMENT_INDEX
        BRACELET_TREATMENT_INDEX = gen_data_args$BRACELET_TREATMENT_INDEX 


        # K-fold CV
        cluster_log_lik = gen_data_args$cluster_log_lik 
        num_excluded_clusters = gen_data_args$num_excluded_clusters
        excluded_clusters = gen_data_args$excluded_clusters


        # Simulation

        beta_control_sd = gen_data_args$beta_control_sd
        beta_ink_effect_sd = gen_data_args$beta_ink_effect_sd
        beta_calendar_effect_sd = gen_data_args$beta_calendar_effect_sd
        beta_bracelet_effect_sd = gen_data_args$beta_bracelet_effect_sd

        dist_beta_v_sd = gen_data_args$dist_beta_v_sd

        structural_beta_county_sd_sd = gen_data_args$structural_beta_county_sd_sd
        structural_beta_cluster_sd_sd = gen_data_args$structural_beta_cluster_sd_sd

        reduced_beta_county_sd_sd = gen_data_args$reduced_beta_county_sd_sd
        reduced_beta_cluster_sd_sd = gen_data_args$reduced_beta_cluster_sd_sd

        beta_far_effect_sd = gen_data_args$beta_far_effect_sd
        beta_far_ink_effect_sd = gen_data_args$beta_far_ink_effect_sd
        beta_far_calendar_effect_sd = gen_data_args$beta_far_calendar_effect_sd
        beta_far_bracelet_effect_sd = gen_data_args$beta_far_bracelet_effect_sd

        is_name_matched = rep(1, num_obs)        

        # transformed data we'll need ------------------------------------------
        num_dist_group_treatments = num_treatments * num_discrete_dist
        cluster_size = table(obs_cluster_id)
        cluster_incentive_treatment_id = cluster_treatment_map[cluster_assigned_dist_group_treatment, 1]
        cluster_dist_treatment_id = cluster_treatment_map[cluster_assigned_dist_group_treatment, 2]



        return(lst(
            num_obs,
            num_clusters,
            num_counties,
            num_treatments,
            num_discrete_dist,
            obs_cluster_id,
            cluster_county_id,
            obs_county_id,
            cluster_treatment_map,
            cluster_assigned_dist_group_treatment,
            cluster_standard_dist,
            
            use_cluster_effects,
            use_county_effects,
            use_binomial,
            use_cost_k_restrictions,
            use_private_incentive_restrictions,
            use_salience_effect,
            use_single_cost_model,
            use_name_matched_obs,
            use_shifting_v_dist,
            multithreaded,
            generate_rep,
            generate_sim,
            fit_model_to_data,
            cross_validate,
            alg_sol_f_tol,
            alg_sol_rel_tol,
            alg_sol_max_steps,
            CALENDAR_TREATMENT_INDEX,
            BRACELET_TREATMENT_INDEX,
            cluster_log_lik,
            num_excluded_clusters,
            excluded_clusters,
            beta_control_sd,
            beta_ink_effect_sd,
            beta_calendar_effect_sd,
            beta_bracelet_effect_sd,
            dist_beta_v_sd,
            structural_beta_county_sd_sd,
            structural_beta_cluster_sd_sd,

            reduced_beta_county_sd_sd,
            reduced_beta_cluster_sd_sd,
            beta_far_effect_sd,
            beta_far_ink_effect_sd,
            beta_far_calendar_effect_sd,
            beta_far_bracelet_effect_sd,

            is_name_matched,
        
            num_dist_group_treatments,
            cluster_size,
            cluster_incentive_treatment_id,
            cluster_dist_treatment_id,


            num_age_groups, 
            obs_age_group,
            use_age_group_gp,
            age_group_alpha_sd,
            age_group_rho_sd,
            sbc = FALSE

        ))
    }
}


meta_gen_params = function(gen_params_args) {
    function(seed, data) {
        set.seed(seed + 1090)
        beta_control = rnorm(
            n = 2,
            mean = 0,
            sd = c(data$beta_control_sd, data$beta_far_effect_sd)
        )

        beta_ink_effect = rnorm(
            n = 2,
            mean = 0,
            sd = c(data$beta_ink_effect_sd, data$beta_far_ink_effect_sd)
        )
        beta_calendar_effect = rnorm(
            n = 2,
            mean = 0,
            sd = c(data$beta_calendar_effect_sd, data$beta_far_calendar_effect_sd)
        )
        beta_bracelet_effect = rnorm(
            n = 2,
            mean = 0,
            sd = c(data$beta_bracelet_effect_sd, data$beta_far_bracelet_effect_sd)
        )

        if (data$use_cluster_effects) {
            beta_cluster_mean = rnorm(n = data$num_clusters)
        } else {
            beta_cluster_mean = NULL
        }
        beta_cluster_sd = abs(rnorm(
            n = 1, 
            mean = 0, 
            sd = data$reduced_beta_cluster_sd_sd))
    

        if (data$use_county_effects) {
            reduced_beta_county_raw = matrix(
                rnorm(
                    n = data$num_counties * data$num_dist_group_treatments,
                    mean = 0,
                    sd = 1
                ),
                nrow = data$num_counties,
                ncol =  data$num_dist_group_treatments 
            ) 
        } else {
            reduced_beta_county_raw = NULL
        }

        reduced_beta_county_sd = abs(rnorm(
            n = data$num_dist_group_treatments,
            mean = 0,
            sd = data$reduced_beta_county_sd_sd
        ))
        beta = matrix(NA, ncol = 1, nrow = data$num_dist_group_treatments)
        for (dist_index in 1:data$num_discrete_dist) {
            beta[(data$num_treatments * (dist_index - 1) + 1):(data$num_treatments * dist_index)] = 
                c(beta_control[dist_index], 
                  beta_ink_effect[dist_index], 
                  beta_calendar_effect[dist_index], 
                  beta_bracelet_effect[dist_index])
        }
        return(lst(
            beta_control,
            beta_ink_effect,
            beta_calendar_effect,
            beta_bracelet_effect,
            beta_cluster_mean,
            beta_cluster_sd,
            reduced_beta_county_raw,
            reduced_beta_county_sd,
            beta
        ))
    }
}




meta_gen_modelled_data = function(gen_modelled_data_args) {
    function(seed, data, params){
        set.seed(seed + 12)

    num_dist_group_treatments = data$num_dist_group_treatments
    treatment_map_design_matrix = matrix(
        0, 
        ncol = num_dist_group_treatments,
        nrow = num_dist_group_treatments)
    treatment_map_design_matrix
    max_close_index = num_dist_group_treatments %/% data$num_discrete_dist
    max_close_index
    treatment_map_design_matrix[, 1] = 1
    treatment_map_design_matrix
    for (treatment_index in 1:num_dist_group_treatments) {
        treatment_map_design_matrix[treatment_index, treatment_index] = 1
        if (treatment_index > max_close_index) {
            treatment_map_design_matrix[treatment_index, max_close_index + 1] = 1
            treatment_map_design_matrix[treatment_index, treatment_index - max_close_index] = 1
        }
    }
    # cluster effects
    cluster_treatment_design_matrix = treatment_map_design_matrix[data$cluster_assigned_dist_group_treatment, ] 
    reduced_treatment_effect = treatment_map_design_matrix %*% params$beta 
    reduced_cluster_benefit_cost = reduced_treatment_effect[data$cluster_assigned_dist_group_treatment]
    if (data$use_cluster_effects) {
        beta_cluster_effect = rnorm(
            n = data$num_clusters,
            mean = params$beta_cluster_mean,
            sd = params$beta_cluster_sd
        )
        reduced_cluster_benefit_cost = reduced_cluster_benefit_cost + beta_cluster_effect
    }
    if (data$use_county_effects) {
        reduced_beta_county = params$reduced_beta_county_raw * matrix(
            params$reduced_beta_county_sd, 
            byrow = TRUE,
            nrow = data$num_counties,  ncol = data$num_dist_group_treatments) 
        county_effects = rowSums(cluster_treatment_design_matrix * reduced_beta_county[data$cluster_county_id])
        reduced_cluster_benefit_cost = reduced_cluster_benefit_cost + county_effects
    }

    reduced_cluster_takeup_prob = pnorm(reduced_cluster_benefit_cost)
    takeup = rbernoulli(n = 1, p = reduced_cluster_takeup_prob[data$obs_cluster_id])
    return(lst(
        takeup
    )) 
    }
}



meta_sample_from_model = function(sample_from_model_args){
    use_cmdstanr = sample_from_model_args$cmdstanr
    function(seed, data, params, modelled_data) {
        data_for_stan = c(data, modelled_data)
        if (use_cmdstanr == TRUE) {
            fit = sample_from_model_args$stan_model$sample(
                data = data_for_stan,
                seed = seed,
                chains = sample_from_model_args$chains,
                iter_warmup = as.integer(sample_from_model_args$iter/2),
                iter_sampling = as.integer(sample_from_model_args$iter/2),
                refresh = ifelse(sample_from_model_args$verbose == 0, 0, 100)
            )
        } else {

            fit =  sampling(
                sample_from_model_args$stan_model,
                data = data_for_stan,
                chains = sample_from_model_args$chains,
                seed = seed,
                iter = sample_from_model_args$iter,
                refresh = ifelse(sample_from_model_args$verbose == 0, 0, 100)
            )
        }
        return(fit)
    }
}






simulate_draw = function(seed = 1234,
                         sim_args,
                         gen_data_args,
                         gen_params_args,
                         gen_modelled_data_args,
                         sample_from_model_args) {
  gen_data = meta_gen_data(gen_data_args = gen_data_args)
  gen_params = meta_gen_params(gen_params_args = gen_params_args)
  gen_modelled_data = meta_gen_modelled_data(gen_modelled_data_args = gen_modelled_data_args)
  sample_from_model = meta_sample_from_model(sample_from_model_args = sample_from_model_args)
  data = gen_data(seed)
  params = gen_params(seed, data)
  modelled_data = gen_modelled_data(seed, data, params)
  if (sim_args$fit_model == TRUE) {
    bayes_fit = sample_from_model(
        seed,
        data,
        params,
        modelled_data
    )
    return(lst(
        bayes_fit,
        params,
        data
    ))
  } 
  if (fit_model == FALSE) {
    return(list(
        params = params,
        data = data,
        modelled_data = modelled_data
    ))
  }
}