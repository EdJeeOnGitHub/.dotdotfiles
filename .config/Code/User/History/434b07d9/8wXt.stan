/*
Parameters Block SBC
*/
// dist_parameters_sec.stan
array[num_discrete_dist] simplex[num_dist_group_mix] group_dist_mix_;

array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_raw_;
array[num_discrete_dist] vector<lower = 0>[num_dist_group_mix] group_dist_sd_;

matrix[use_dist_county_effects ? num_discrete_dist : 0, num_counties] county_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] county_dist_effect_sd_;

matrix[use_dist_cluster_effects ? num_discrete_dist : 0, num_clusters] cluster_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] cluster_dist_effect_sd_;
  






// Levels: control ink calendar bracelet
real beta_control_;
real beta_ink_effect_;
real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_calendar_effect_;
real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_bracelet_effect_;

matrix[use_cluster_effects ? num_clusters : 0, num_treatments] structural_beta_cluster_raw_;
row_vector<lower = 0>[use_cluster_effects ? num_treatments : 0] structural_beta_cluster_sd_;

matrix[use_county_effects ? num_counties : 0, num_treatments] structural_beta_county_raw_;
row_vector<lower = 0>[use_county_effects ? num_treatments : 0] structural_beta_county_sd_;

// Salience
real<lower = 0> beta_salience_;
real<lower = 0> dist_beta_salience_;
real<lower = 0> dist_quadratic_beta_salience_;

// Reputational Returns
real<lower = 0> base_mu_rep_;
vector<lower = 0>[use_homoskedastic_shocks ? 1 : num_treatment_shocks] raw_u_sd_;

matrix[!use_mu_cluster_effects || (suppress_reputation && !use_dist_salience) ? 0 : num_clusters, num_treatments] mu_cluster_effects_raw_;
row_vector<lower = 0>[!use_mu_cluster_effects || (suppress_reputation && !use_dist_salience) ? 0 : num_treatments] mu_cluster_effects_sd_;

matrix[!use_mu_county_effects || (suppress_reputation && !use_dist_salience) ? 0 : num_counties, num_treatments] mu_county_effects_raw_;
row_vector<lower = 0>[!use_mu_county_effects || (suppress_reputation && !use_dist_salience) ? 0 : num_treatments] mu_county_effects_sd_;

// Linear Parametric Cost
vector<lower = (use_dist_salience ? 0 : negative_infinity())>[num_treatments_param] dist_beta_v_; // Linear distance*treatment effects

matrix[use_param_dist_cluster_effects ? num_clusters : 0, num_treatments_param] dist_beta_cluster_raw_;
row_vector<lower = 0>[use_param_dist_cluster_effects ? num_treatments_param : 0] dist_beta_cluster_sd_;

matrix[use_param_dist_county_effects ? num_counties : 0, num_treatments_param] dist_beta_county_raw_;
row_vector<lower = 0>[use_param_dist_county_effects ? num_treatments_param : 0] dist_beta_county_sd_;

// Quadratic Cost Model

vector<lower = 0>[num_treatments_param_quadratic] dist_quadratic_beta_v_; // Quadratic distance*treatment effects


// WTP valuation parameters
real<lower = 0> wtp_value_utility_;


/*
Transformed Parameters Block SBC
*/
// dist_transformed_parameters.stan
array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_;
matrix[num_discrete_dist, num_counties] county_dist_effect_ = rep_matrix(0, num_discrete_dist, num_counties);
matrix[num_discrete_dist, num_clusters] cluster_dist_effect_ = rep_matrix(0, num_discrete_dist, num_clusters);

for (dist_group_index in 1:num_discrete_dist) { 
  if (use_dist_county_effects) { 
    county_dist_effect_[dist_group_index] = county_dist_effect_raw_[dist_group_index] * county_dist_effect_sd_[dist_group_index];  
  }
  
  if (use_dist_cluster_effects) { 
    cluster_dist_effect_[dist_group_index] = cluster_dist_effect_raw_[dist_group_index] * cluster_dist_effect_sd_[dist_group_index];  
  }
  
  group_dist_mean_[dist_group_index] = hyper_dist_mean_mean + hyper_dist_mean_sd * group_dist_mean_raw_[dist_group_index]; 
}


// transformed parameters, main file
vector[num_dist_group_treatments] beta_;

vector[num_dist_group_treatments] structural_treatment_effect_;
vector[num_clusters] structural_cluster_benefit_cost_;
matrix[num_clusters, num_dist_group_treatments] structural_beta_cluster_ = rep_matrix(0, num_clusters, num_dist_group_treatments);
matrix[num_counties, num_dist_group_treatments] structural_beta_county_ = rep_matrix(0, num_counties, num_dist_group_treatments);

vector<lower = 0>[!suppress_reputation || use_dist_salience ? num_clusters : 0] obs_cluster_mu_rep_;

vector[num_clusters] structural_cluster_obs_v_ = rep_vector(0, num_clusters);
row_vector<lower = 0, upper = 1>[fit_model_to_data ? num_clusters : 0] structural_cluster_takeup_prob_;


vector<lower = 0>[num_treatments_param_kappa] dist_cost_k_;
vector[num_dist_group_treatments] linear_dist_cost_ = rep_vector(0, num_dist_group_treatments);
vector[num_dist_group_treatments] quadratic_dist_cost_ = rep_vector(0, num_dist_group_treatments);
matrix[num_clusters, num_dist_group_treatments] cluster_linear_dist_cost_ = rep_matrix(0, num_clusters, num_dist_group_treatments);
matrix[num_clusters, num_dist_group_treatments] cluster_quadratic_dist_cost_ = rep_matrix(0, num_clusters, num_dist_group_treatments);
matrix[num_treatments, num_knots_v] u_splines_v_ = rep_matrix(0, num_treatments, num_knots_v);
vector[num_clusters] cluster_dist_cost_ = rep_vector(0, num_clusters);

vector<lower = 0>[num_treatments] u_sd_;
vector<lower = 0>[num_treatments] total_error_sd_;
  
if (use_homoskedastic_shocks) {
    u_sd_ = rep_vector(use_wtp_model ? sqrt(square(raw_u_sd_[1]) + square(wtp_sigma_ * wtp_value_utility_)) : raw_u_sd_[1], num_treatments);  
} else {
    u_sd_[{ 1, 2, BRACELET_TREATMENT_INDEX }] = raw_u_sd_[{ 1, 2, use_wtp_model ? num_treatment_shocks : BRACELET_TREATMENT_INDEX }];
    u_sd_[CALENDAR_TREATMENT_INDEX] = use_wtp_model ? raw_u_sd_[num_treatment_shocks] : raw_u_sd_[CALENDAR_TREATMENT_INDEX];  
}
    
total_error_sd_ = sqrt(1 + square(u_sd_));

for (dist_index in 1:num_discrete_dist) {
    if (dist_index > 1) {
        beta_[(num_treatments + 1):] = rep_vector(0, num_treatments); 
    } else if (use_wtp_model) { 
        beta_[1:2] = [ beta_control_, beta_ink_effect_ ]';
        beta_[CALENDAR_TREATMENT_INDEX] = beta_bracelet_effect_ + wtp_value_utility_ * hyper_wtp_mu_;
        beta_[BRACELET_TREATMENT_INDEX] = beta_bracelet_effect_;
    } else {
        beta_[1:num_treatments] = [ beta_control_, beta_ink_effect_, beta_calendar_effect_, beta_bracelet_effect_ ]';
    }
}

structural_treatment_effect_ = restricted_treatment_map_design_matrix * beta_;

if (!suppress_reputation || use_dist_salience) { 
    obs_cluster_mu_rep_ = calculate_mu_rep(
        cluster_incentive_treatment_id, cluster_standard_dist, 
        base_mu_rep_, 1, beliefs_treatment_map_design_matrix, centered_cluster_beta_1ord_, centered_cluster_dist_beta_1ord_);

    if (use_mu_cluster_effects) {
        // I shouldn't be using this anymore since we're using beliefs 
        
        // matrix[num_clusters, num_treatments] mu_cluster_effects =  mu_cluster_effects_raw .* rep_matrix(mu_cluster_effects_sd, num_clusters);
        // 
        // cluster_mu_rep = cluster_mu_rep .* exp(mu_cluster_effects);
    }

    if (use_mu_county_effects) {
        // matrix[num_counties, num_treatments] mu_county_effects =  mu_county_effects_raw .* rep_matrix(mu_county_effects_sd, num_counties);
        // 
        // cluster_mu_rep = cluster_mu_rep .* exp(mu_county_effects[cluster_county_id]);
    } 
}

  if (in_array(use_cost_model, { COST_MODEL_TYPE_PARAM_LINEAR_SALIENCE, COST_MODEL_TYPE_PARAM_QUADRATIC_SALIENCE, COST_MODEL_TYPE_SEMIPARAM_SALIENCE })) {
    // linear_dist_cost = rep_vector(dist_beta_v[1], num_dist_group_treatments) + dist_beta_salience * mu_rep';  // append_col(mu_rep, mu_rep)';
    reject("Salience not yet supported."); // TODO Fix this
    
    if (use_param_dist_cluster_effects) {
      cluster_linear_dist_cost_ += rep_matrix(dist_beta_cluster_raw_[, 1] * dist_beta_cluster_sd_[1], num_dist_group_treatments);
    }
    
    if (use_param_dist_county_effects) {
      cluster_linear_dist_cost_ += rep_matrix((dist_beta_county_raw_[, 1] * dist_beta_county_sd_[1])[cluster_county_id], num_dist_group_treatments);
    }
    
    if (use_cost_model == COST_MODEL_TYPE_PARAM_QUADRATIC_SALIENCE) {
      // quadratic_dist_cost = rep_vector(dist_quadratic_beta_v[1], num_dist_group_treatments) + dist_quadratic_beta_salience * mu_rep'; // append_col(mu_rep, mu_rep)';
      reject("Salience not yet supported."); // TODO Fix this
      
      if (use_param_dist_cluster_effects) {
        // TODO
      }
      
      if (use_param_dist_county_effects) {
        // TODO
      }
    }
  } else {
    if (use_single_cost_model) {
      linear_dist_cost_ = rep_vector(dist_beta_v_[1], num_dist_group_treatments);
      
      if (use_param_dist_cluster_effects) {
        cluster_linear_dist_cost_ += rep_matrix(dist_beta_cluster_raw_[, 1] * dist_beta_cluster_sd_[1], num_dist_group_treatments);
      }
      
      if (use_param_dist_county_effects) {
        cluster_linear_dist_cost_ += rep_matrix((dist_beta_county_raw_[, 1] * dist_beta_county_sd_[1])[cluster_county_id], num_dist_group_treatments);
      }
      
      if (num_treatments_param_quadratic > 0) {
        quadratic_dist_cost_ = rep_vector(dist_quadratic_beta_v_[1], num_dist_group_treatments);
        
        if (use_param_dist_cluster_effects) {
          // TODO
        }
        
        if (use_param_dist_county_effects) {
          // TODO
        }
      }
    } else {
      linear_dist_cost_ = treatment_map_design_matrix * append_row(append_row(dist_beta_v_, 0), dist_beta_v_[2:]);
      
      if (use_param_dist_cluster_effects) {
        cluster_linear_dist_cost_ += dist_beta_cluster_raw_ .* rep_matrix(dist_beta_cluster_sd_, num_clusters);
      }
      
      if (use_param_dist_county_effects) {
        cluster_linear_dist_cost_ += (dist_beta_county_raw_ .* rep_matrix(dist_beta_county_sd_, num_counties))[cluster_county_id];
      }
      
      if (num_treatments_param_quadratic > 0) {
        quadratic_dist_cost_ = treatment_map_design_matrix * append_row(append_row(dist_quadratic_beta_v_, 0), dist_quadratic_beta_v_[2:]);
        
        if (use_param_dist_cluster_effects) {
          // TODO
        }
        
        if (use_param_dist_county_effects) {
          // TODO
        }
      }
    }
  } 

  cluster_linear_dist_cost_ += rep_matrix(linear_dist_cost_', num_clusters);
  cluster_quadratic_dist_cost_ += rep_matrix(quadratic_dist_cost_', num_clusters);
  
  if (use_semiparam) {
    for (treatment_index in 1:num_treatments_semiparam) {
      if (use_dist_salience) {
        // u_splines_v[treatment_index] = u_splines_v_raw[1] * u_splines_v_sigma * mu_rep[treatment_index]; // BUGBUG This shouldn't work anymore
        reject("Salience not yet supported."); // TODO Fix this
      } else {
        u_splines_v_[treatment_index] = u_splines_v_raw_[treatment_index] * u_splines_v_sigma_;
      }
    }        
  } 
    
  cluster_dist_cost_ = param_dist_cost_with_splines(
                                      cluster_standard_dist, 
                                      to_vector(cluster_linear_dist_cost_)[long_cluster_by_treatment_index],
                                      to_vector(cluster_quadratic_dist_cost_)[long_cluster_by_treatment_index],
                                      u_splines_v_[cluster_incentive_treatment_id],
                                      Z_splines_v_[cluster_incentive_treatment_id]);
  
  structural_cluster_benefit_cost_ = structural_treatment_effect_[cluster_assigned_dist_group_treatment] - cluster_dist_cost_;

  
  if (use_cluster_effects) {
    vector[num_clusters] cluster_effects_;
    
    structural_beta_cluster_[, 1:num_treatments] = structural_beta_cluster_raw_ .* rep_matrix(structural_beta_cluster_sd_, num_clusters);
    
    if (use_wtp_model) { // Bracelet and Calendar are the same
      structural_beta_cluster_[, 3] = structural_beta_cluster_[, 4];
    } 
    
    cluster_effects_ = rows_dot_product(cluster_treatment_design_matrix, structural_beta_cluster_); 
    structural_cluster_benefit_cost_ += cluster_effects_;
  }
  
  if (use_county_effects) {
    vector[num_clusters] county_effects_;
    
    structural_beta_county_[, 1:num_treatments] = structural_beta_county_raw_ .* rep_matrix(structural_beta_county_sd_, num_counties);
    
    if (use_wtp_model) { // Calendar = Bracelet + strata_effect
      structural_beta_county_[, 3] = structural_beta_county_[, 4] + wtp_value_utility_ * strata_effect_wtp_mu_; 
    } 
    
    county_effects_ = rows_dot_product(cluster_treatment_design_matrix, structural_beta_county_[cluster_county_id]); 
    structural_cluster_benefit_cost_ += county_effects_;
  }

  if (fit_model_to_data) {

    if (suppress_reputation) {
      structural_cluster_obs_v_ = - structural_cluster_benefit_cost_;
    } else {
      if (multithreaded) {
        structural_cluster_obs_v_ = map_find_fixedpoint_solution(
          structural_cluster_benefit_cost_, 
          obs_cluster_mu_rep_,
          total_error_sd_[cluster_incentive_treatment_id],
          u_sd_[cluster_incentive_treatment_id],
          
          use_u_in_delta,
          alg_sol_rel_tol, // 1e-10,
          alg_sol_f_tol, // 1e-5,
          alg_sol_max_steps
        ); 
      } else {
        for (cluster_index in 1:num_clusters) {
          structural_cluster_obs_v_[cluster_index] = find_fixedpoint_solution(
            structural_cluster_benefit_cost_[cluster_index],
            obs_cluster_mu_rep_[cluster_index],
            total_error_sd_[cluster_incentive_treatment_id[cluster_index]],
            u_sd_[cluster_incentive_treatment_id[cluster_index]],

            use_u_in_delta,
            alg_sol_rel_tol, // 1e-10,
            alg_sol_f_tol, // 1e-5,
            alg_sol_max_steps
          );
        }
      }
    }
    
    structural_cluster_takeup_prob_ = Phi_approx(- structural_cluster_obs_v_ ./ total_error_sd_[cluster_incentive_treatment_id])';
  }