functions {
#include util.stan
#include beliefs_functions.stan
#include takeup_functions.stan
}

data {
#include base_data_sec.stan
#include takeup_data_sec.stan
#include wtp_data.stan
#include beliefs_data_sec.stan
#include dist_data_sec.stan

  int MIN_COST_MODEL_TYPE_VALUE;
  int MAX_COST_MODEL_TYPE_VALUE;
  int<lower = MIN_COST_MODEL_TYPE_VALUE, upper = MAX_COST_MODEL_TYPE_VALUE> COST_MODEL_TYPE_PARAM_LINEAR;
  int<lower = MIN_COST_MODEL_TYPE_VALUE, upper = MAX_COST_MODEL_TYPE_VALUE> COST_MODEL_TYPE_PARAM_QUADRATIC;
  int<lower = MIN_COST_MODEL_TYPE_VALUE, upper = MAX_COST_MODEL_TYPE_VALUE> COST_MODEL_TYPE_DISCRETE;
  
  int<lower = 0, upper = 1> use_wtp_model;
  int<lower = 0, upper = 1> use_homoskedastic_shocks;
  
  int<lower = MIN_COST_MODEL_TYPE_VALUE, upper = MAX_COST_MODEL_TYPE_VALUE> use_cost_model;
  int<lower = 0, upper = 1> suppress_reputation;
  int<lower = 0, upper = 1> use_u_in_delta;
  
  int<lower = 0, upper = 1> use_param_dist_cluster_effects; // These are used for parameteric (linear, quadratic) distance cost models only
  int<lower = 0, upper = 1> use_param_dist_county_effects;
  
  // Rate of Change
  
  int<lower = 1, upper = num_treatments> roc_compare_treatment_id_left;
  int<lower = 1, upper = num_treatments> roc_compare_treatment_id_right;
  int<lower = 0> num_roc_distances;
  vector<lower = 0>[num_roc_distances] roc_distances; // These should be standardized already
 
  // Sim Delta
  
  int<lower = 0> num_sim_delta_w;
  vector[num_sim_delta_w] sim_delta_w;
  
  // Hyperparam
  
  real<lower = 0> mu_rep_sd;
}

transformed data {
#include base_transformed_data.stan 
#include wtp_transformed_data.stan
#include takeup_transformed_data.stan
#include beliefs_transformed_data.stan

  array[1] real dummy_xr = { 1.0 }; 
  array[1] int dummy_xi = { 1 }; 
  
  int<lower = 0, upper = num_treatments> num_treatment_shocks = num_treatments - (use_wtp_model ? 1 : 0);
  
  int num_dist_param = 1;
  int num_dist_param_quadratic = use_cost_model == COST_MODEL_TYPE_PARAM_QUADRATIC ? 1 : 0; 
  
  if (num_age_groups > 1) {
    reject("Age groups not suported in structural model.");
  }
}

parameters {
#include dist_parameters_sec.stan
#include wtp_parameters.stan
#include beliefs_parameters_sec.stan
  
  // Levels: control ink calendar bracelet
  real beta_intercept;
  real beta_ink_effect;
  real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_calendar_effect;
  real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_bracelet_effect;
  
  matrix[use_cluster_effects ? num_clusters : 0, num_treatments] structural_beta_cluster_raw;
  row_vector<lower = 0>[use_cluster_effects ? num_treatments : 0] structural_beta_cluster_sd;
  
  matrix[use_county_effects ? num_counties : 0, num_treatments] structural_beta_county_raw;
  row_vector<lower = 0>[use_county_effects ? num_treatments : 0] structural_beta_county_sd;
  
  // Reputational Returns
  
  real<lower = 0> base_mu_rep;
  vector<lower = 0>[use_homoskedastic_shocks ? 1 : num_treatment_shocks] raw_u_sd;
  
  // Linear Parametric Cost
  
  vector[num_dist_param] dist_beta_v; // Linear distance*treatment effects
  
  matrix[use_param_dist_cluster_effects ? num_clusters : 0, num_dist_param] dist_beta_cluster_raw;
  row_vector<lower = 0>[use_param_dist_cluster_effects ? num_dist_param : 0] dist_beta_cluster_sd;
  
  matrix[use_param_dist_county_effects ? num_counties : 0, num_dist_param] dist_beta_county_raw;
  row_vector<lower = 0>[use_param_dist_county_effects ? num_dist_param : 0] dist_beta_county_sd;
  
  // Quadratic Cost Model
  
  vector<lower = 0>[num_dist_param_quadratic] dist_quadratic_beta_v; // Quadratic distance*treatment effects
  
  // WTP valuation parameters
  real<lower = 0> wtp_value_utility;
}

transformed parameters {
#include dist_transformed_parameters.stan
#include wtp_transformed_parameters.stan
#include beliefs_transformed_parameters.stan
  
  vector[num_dist_group_treatments] beta;
  
  vector[num_dist_group_treatments] structural_treatment_effect;
  vector[num_clusters] structural_cluster_benefit_cost;
  matrix[num_clusters, num_dist_group_treatments] structural_beta_cluster = rep_matrix(0, num_clusters, num_dist_group_treatments);
  matrix[num_counties, num_dist_group_treatments] structural_beta_county = rep_matrix(0, num_counties, num_dist_group_treatments);
 
  vector<lower = 0>[suppress_reputation ? 0 : num_clusters] obs_cluster_mu_rep;
  
  vector[num_clusters] structural_cluster_obs_v = rep_vector(0, num_clusters);
  row_vector<lower = 0, upper = 1>[fit_model_to_data ? num_clusters : 0] structural_cluster_takeup_prob;
  
  vector[num_dist_group_treatments] linear_dist_cost = rep_vector(0, num_dist_group_treatments);
  vector[num_dist_group_treatments] quadratic_dist_cost = rep_vector(0, num_dist_group_treatments);
  matrix[num_clusters, num_dist_group_treatments] cluster_linear_dist_cost = rep_matrix(0, num_clusters, num_dist_group_treatments);
  matrix[num_clusters, num_dist_group_treatments] cluster_quadratic_dist_cost = rep_matrix(0, num_clusters, num_dist_group_treatments);
  vector[num_clusters] cluster_dist_cost = rep_vector(0, num_clusters);
  
  vector<lower = 0>[num_treatments] u_sd;
  vector<lower = 0>[num_treatments] total_error_sd;
  
  if (use_homoskedastic_shocks) {
    u_sd = rep_vector(use_wtp_model ? sqrt(square(raw_u_sd[1]) + square(wtp_sigma * wtp_value_utility)) : raw_u_sd[1], num_treatments);  
  } else {
    u_sd[{ 1, 2, BRACELET_TREATMENT_INDEX }] = raw_u_sd[{ 1, 2, use_wtp_model ? num_treatment_shocks : BRACELET_TREATMENT_INDEX }];
    u_sd[CALENDAR_TREATMENT_INDEX] = use_wtp_model ? raw_u_sd[num_treatment_shocks] : raw_u_sd[CALENDAR_TREATMENT_INDEX];  
  }
    
  total_error_sd = sqrt(1 + square(u_sd));
  
  for (dist_index in 1:num_discrete_dist) {
    if (dist_index > 1) {
      beta[(num_treatments + 1):] = rep_vector(0, num_treatments); 
    } else if (use_wtp_model) { 
      beta[1:2] = [ beta_intercept, beta_ink_effect ]';
      beta[CALENDAR_TREATMENT_INDEX] = beta_bracelet_effect + wtp_value_utility * hyper_wtp_mu;
      beta[BRACELET_TREATMENT_INDEX] = beta_bracelet_effect;
    } else {
      beta[1:num_treatments] = [ beta_intercept, beta_ink_effect, beta_calendar_effect, beta_bracelet_effect ]';
    }
  }
 
  structural_treatment_effect = treatment_map_design_matrix * beta;
  
  // Levels: control ink calendar bracelet
 
  if (!suppress_reputation) { 
    obs_cluster_mu_rep = calculate_mu_rep(
      cluster_incentive_treatment_id, cluster_standard_dist, 
      base_mu_rep, 1, beliefs_treatment_map_design_matrix, centered_cluster_beta_1ord, centered_cluster_dist_beta_1ord);
  }
  
  linear_dist_cost = rep_vector(dist_beta_v[1], num_dist_group_treatments);
  
  if (use_param_dist_cluster_effects) {
    cluster_linear_dist_cost += rep_matrix(dist_beta_cluster_raw[, 1] * dist_beta_cluster_sd[1], num_dist_group_treatments);
  }
  
  if (use_param_dist_county_effects) {
    cluster_linear_dist_cost += rep_matrix((dist_beta_county_raw[, 1] * dist_beta_county_sd[1])[cluster_county_id], num_dist_group_treatments);
  }
  
  if (num_dist_param_quadratic > 0) {
    quadratic_dist_cost = rep_vector(dist_quadratic_beta_v[1], num_dist_group_treatments);
    
    if (use_param_dist_cluster_effects) {
      // TODO
    }
    
    if (use_param_dist_county_effects) {
      // TODO
    }
  }
  
  cluster_linear_dist_cost += rep_matrix(linear_dist_cost', num_clusters);
  cluster_quadratic_dist_cost += rep_matrix(quadratic_dist_cost', num_clusters);
  
    
  cluster_dist_cost = param_dist_cost(
                                      cluster_standard_dist, 
                                      to_vector(cluster_linear_dist_cost)[long_cluster_by_treatment_index],
                                      to_vector(cluster_quadratic_dist_cost)[long_cluster_by_treatment_index]);
  
  structural_cluster_benefit_cost = structural_treatment_effect[cluster_assigned_dist_group_treatment] - cluster_dist_cost;
  
  if (use_cluster_effects) {
    vector[num_clusters] cluster_effects;
    
    structural_beta_cluster[, 1:num_treatments] = structural_beta_cluster_raw .* rep_matrix(structural_beta_cluster_sd, num_clusters);
    
    if (use_wtp_model) { // Bracelet and Calendar are the same
      structural_beta_cluster[, 3] = structural_beta_cluster[, 4];
    } 
    
    cluster_effects = rows_dot_product(cluster_treatment_design_matrix, structural_beta_cluster); 
    structural_cluster_benefit_cost += cluster_effects;
  }
  
  if (use_county_effects) {
    vector[num_clusters] county_effects;
    
    structural_beta_county[, 1:num_treatments] = structural_beta_county_raw .* rep_matrix(structural_beta_county_sd, num_counties);
    
    if (use_wtp_model) { // Calendar = Bracelet + strata_effect
      structural_beta_county[, 3] = structural_beta_county[, 4] + wtp_value_utility * strata_effect_wtp_mu; 
    } 
    
    county_effects = rows_dot_product(cluster_treatment_design_matrix, structural_beta_county[cluster_county_id]); 
    structural_cluster_benefit_cost += county_effects;
  }

  if (fit_model_to_data) {
    if (suppress_reputation) {
      structural_cluster_obs_v = - structural_cluster_benefit_cost;
    } else {
      if (multithreaded) {
        structural_cluster_obs_v = map_find_fixedpoint_solution(
          structural_cluster_benefit_cost, 
          obs_cluster_mu_rep,
          total_error_sd[cluster_incentive_treatment_id],
          u_sd[cluster_incentive_treatment_id],
          
          use_u_in_delta,
          alg_sol_rel_tol, // 1e-10,
          alg_sol_f_tol, // 1e-5,
          alg_sol_max_steps
        ); 
      } else {
        for (cluster_index in 1:num_clusters) {
          structural_cluster_obs_v[cluster_index] = find_fixedpoint_solution(
            structural_cluster_benefit_cost[cluster_index],
            obs_cluster_mu_rep[cluster_index],
            total_error_sd[cluster_incentive_treatment_id[cluster_index]],
            u_sd[cluster_incentive_treatment_id[cluster_index]],

            use_u_in_delta,
            alg_sol_rel_tol, // 1e-10,
            alg_sol_f_tol, // 1e-5,
            alg_sol_max_steps
          );
        }
      }
    }
    
    structural_cluster_takeup_prob = Phi_approx(- structural_cluster_obs_v ./ total_error_sd[cluster_incentive_treatment_id])';
  }
}

model {
#include wtp_model_section.stan
#include beliefs_model_sec.stan
#include dist_model_sec.stan
  
  wtp_value_utility ~ normal(0, 0.1);

  beta_intercept ~ normal(0, beta_intercept_sd);
  
  beta_ink_effect ~ normal(0, beta_ink_effect_sd);
  beta_calendar_effect ~ normal(0, beta_calendar_effect_sd);
  beta_bracelet_effect ~ normal(0, beta_bracelet_effect_sd);
  
  
  base_mu_rep ~ normal(0, mu_rep_sd);
  
  if (num_dist_param > 0) {
    dist_beta_v ~ normal(0, dist_beta_v_sd);
    
    if (use_param_dist_cluster_effects) {
      to_vector(dist_beta_cluster_raw) ~ normal(0, 1);
      dist_beta_cluster_sd ~ normal(0, 0.25);
    }
    
    if (use_param_dist_county_effects) {
      to_vector(dist_beta_county_raw) ~ normal(0, 1);
      dist_beta_county_sd ~ normal(0, 0.25);
    }
  }
  
  if (num_dist_param_quadratic > 0) {
    dist_quadratic_beta_v ~ normal(0, 1);
  }
  
  if (use_cluster_effects) {
    to_vector(structural_beta_cluster_raw) ~ std_normal();
    structural_beta_cluster_sd ~ normal(0, structural_beta_cluster_sd_sd);
  }
  
  if (use_county_effects) {
    to_vector(structural_beta_county_raw) ~ std_normal();
    structural_beta_county_sd ~ normal(0, structural_beta_county_sd_sd);
  }
  
  raw_u_sd ~ normal(0, 1);
  
  if (fit_model_to_data) {
    // Take-up Likelihood 
    if (use_binomial) {
      // Age groups not supported
      cluster_takeup_count[included_clusters, 1] ~ binomial(cluster_size[included_clusters, 1], structural_cluster_takeup_prob[included_clusters]);
    } else {
      takeup[included_monitored_obs] ~ bernoulli(structural_cluster_takeup_prob[obs_cluster_id[included_monitored_obs]]);
    }
  }
}

generated quantities {
#include dist_generated_quantities.stan
#include beliefs_generated_quantities.stan

  matrix[num_clusters, num_dist_group_treatments] structural_cluster_benefit = 
        rep_matrix(structural_treatment_effect', num_clusters) + 
        (structural_beta_cluster + structural_beta_county[cluster_county_id]) * treatment_map_design_matrix';
  
  array[num_dist_group_treatments] vector[num_clusters] cluster_cf_benefit_cost; 
  
  array[num_dist_group_treatments, num_treatments] vector[num_clusters] cluster_cf_cutoff; 
  
  vector[generate_rep ? num_clusters : 0] cluster_rep_benefit_cost; 
  vector[generate_rep ? num_clusters : 0] cluster_rep_cutoff; 
  
  vector[num_sim_delta_w] sim_delta;
  
#include wtp_generated_quantities.stan
#include takeup_struct_cv.stan
#include takeup_struct_quantities.stan

  {
    int treatment_cluster_pos = 1;
    int cluster_treatment_cf_pos = 1;
    
    for (treatment_index in 1:num_dist_group_treatments) {
      vector[num_clusters] treatment_dist_cost;
     
      int curr_assigned_treatment = cluster_treatment_map[treatment_index, 1];
      int curr_assigned_dist_group = cluster_treatment_map[treatment_index, 2];
      
      treatment_dist_cost = param_dist_cost(
                                            all_cluster_standard_dist[, curr_assigned_dist_group],
                                            cluster_linear_dist_cost[, treatment_index],
                                            cluster_quadratic_dist_cost[, treatment_index]);
                                                                 
      cluster_cf_benefit_cost[treatment_index] = structural_cluster_benefit[, treatment_index] - treatment_dist_cost;
      
      for (mu_treatment_index in 1:num_treatments) {
        vector[num_clusters] curr_cluster_mu_rep = calculate_mu_rep(
          { mu_treatment_index }, all_cluster_standard_dist[, curr_assigned_dist_group], 
          base_mu_rep, 1, beliefs_treatment_map_design_matrix, centered_cluster_beta_1ord, centered_cluster_dist_beta_1ord);
        
        if (multithreaded) {
          cluster_cf_cutoff[treatment_index, mu_treatment_index] = map_find_fixedpoint_solution(
            cluster_cf_benefit_cost[treatment_index],
            curr_cluster_mu_rep,
            rep_vector(total_error_sd[curr_assigned_treatment], num_clusters),
            rep_vector(u_sd[curr_assigned_treatment], num_clusters),
            
            use_u_in_delta, 
            alg_sol_rel_tol, 
            alg_sol_f_tol, 
            alg_sol_max_steps
          );
        } else {
          for (cluster_index in 1:num_clusters) {
            cluster_cf_cutoff[treatment_index, mu_treatment_index, cluster_index] = find_fixedpoint_solution(
              cluster_cf_benefit_cost[treatment_index, cluster_index],
              curr_cluster_mu_rep[cluster_index],
              total_error_sd[curr_assigned_treatment],
              u_sd[curr_assigned_treatment],
              
              use_u_in_delta, 
              alg_sol_rel_tol, 
              alg_sol_f_tol, 
              alg_sol_max_steps
            ); 
          }
        }
      }
    }
  }
  
  // Replicated Data  
  if (generate_rep) {
    for (cluster_index in 1:num_clusters) {
      vector[num_dist_group_treatments] rep_beta_cluster = rep_vector(0, num_dist_group_treatments);
        
      vector[num_dist_group_treatments] rep_beta_county = rep_vector(0, num_dist_group_treatments);
      
      int curr_assigned_treatment = cluster_incentive_treatment_id[cluster_index];
      
      real rep_dist_cost;
      
      real rep_linear_dist_cost = linear_dist_cost[cluster_assigned_dist_group_treatment[cluster_index]];
      real rep_quadratic_dist_cost = quadratic_dist_cost[cluster_assigned_dist_group_treatment[cluster_index]];
      
      rep_beta_cluster[1:num_treatments] = use_cluster_effects ? to_vector(normal_rng(rep_array(0, num_treatments), structural_beta_cluster_sd')) : rep_vector(0, num_treatments);
      rep_beta_cluster[(num_treatments + 2):num_dist_group_treatments] = rep_beta_cluster[2:num_treatments];
      
      rep_beta_county[1:num_treatments] = use_county_effects ? to_vector(normal_rng(rep_array(0, num_treatments), structural_beta_county_sd')) : rep_vector(0, num_treatments);
      rep_beta_county[(num_treatments + 2):num_dist_group_treatments] = rep_beta_county[2:num_treatments];
        
      if (use_param_dist_cluster_effects) {
        rep_linear_dist_cost += normal_rng(0, dist_beta_cluster_sd[1]); 
      }
      
      if (use_param_dist_county_effects) {
        rep_linear_dist_cost += normal_rng(0, dist_beta_county_sd[1]); 
      }
        
      rep_dist_cost = param_dist_cost(
                                      [ cluster_standard_dist[cluster_index] ]', 
                                      [ rep_linear_dist_cost ]',
                                      [ rep_quadratic_dist_cost ]')[1]; 
      
      cluster_rep_benefit_cost[cluster_index] = structural_treatment_effect[cluster_assigned_dist_group_treatment[cluster_index]] 
        + treatment_map_design_matrix[cluster_assigned_dist_group_treatment[cluster_index]] * (rep_beta_cluster + rep_beta_county) - rep_dist_cost;
        
      cluster_rep_cutoff[cluster_index] = find_fixedpoint_solution(
          cluster_rep_benefit_cost[cluster_index],
          obs_cluster_mu_rep[cluster_index],
          total_error_sd[curr_assigned_treatment],
          u_sd[curr_assigned_treatment],
          
          use_u_in_delta,
          alg_sol_rel_tol,
          alg_sol_f_tol,
          alg_sol_max_steps
        );
    }
  }
  
  for (sim_delta_index in 1:num_sim_delta_w) {
    sim_delta[sim_delta_index] = expected_delta(sim_delta_w[sim_delta_index], total_error_sd[1], u_sd[1], dummy_xr, dummy_xi);
  }
}
