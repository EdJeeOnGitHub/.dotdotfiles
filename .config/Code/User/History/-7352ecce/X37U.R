# Create DGP functions for simulation based calibration


meta_gen_data = function(gen_data_args) {
    function(seed) {
        set.seed(seed + 189)


        ## base_data_sec.stan section -----------------------------------------------
        num_obs = gen_data_args$num_obs
        num_clusters = gen_data_args$num_clusters
        num_counties = gen_data_args$num_counties
        num_treatments = gen_data_args$num_treatments
        num_discrete_dist = gen_data$num_discrete_dist

        # Note implies clusters approx equal size - no skewing here.
        obs_cluster_id = sample(1:num_clusters, size = num_obs, replace = TRUE)
        cluster_county_id = sample(1:num_counties, size = num_clusters, replace = TRUE)
        obs_county_id = clust_county_id[obs_cluster_id]

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
            mean = gen_data_args$mean,
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

        # TODO/Remember `takeup`, `is_name_matched` to go into modelled_data

        # Semiparametric Cost Model (Splines) 
        # ed: I think this is superfluous?
        num_knots_v = gen_data_args$num_knots_v
        Z_splines_v = gen_data_args$Z_splines_v

        u_splines_v_sigma_sd = gen_data_args$u_splines_v_sigma_sd

        # K-fold CV
        cluster_log_lik = gen_data_args$cluster_log_lik 
        num_excluded_clusters = gen_data_args$num_excluded_clusters
        excluded_clusters = gen_data_args$excluded_clusters


        # Simulation
        num_grid_obs = gen_data_args$num_grid_obs
        num_small_grid_obs = gen_data_args$num_small_grid_obs
        grid_dist = gen_data_args$grid_dist
        small_grid_dist = gen_data_args$small_grid_dist
        Z_grid_v = gen_data_args$Z_grid_v

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
            num_knots_v,
            Z_splines_v,
            u_splines_v_sigma_sd,
            cluster_log_lik,
            num_excluded_clusters,
            excluded_clusters,
            num_grid_obs,
            num_small_grid_obs,
            grid_dist,
            small_grid_dist,
            Z_grid_v,
            beta_control_sd,
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
            beta_far_bracelet_effect_sd
        ))
    }
}


meta_gen_params = function(gen_params_args) {


}




