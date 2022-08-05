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
  


// wtp_parameters.stan
real hyper_wtp_mu_;

vector[use_strata_levels ? num_strata : 0] raw_strata_wtp_mu_;
real<lower = 0> strata_wtp_mu_tau_;

real<lower = 0> wtp_sigma_;

// here ed
// simulate wtp_parameters.stan
hyper_wtp_mu_ = normal_rng(0, 2);

if (use_strata_levels) {
  for (k in 1:num_strata) {
    raw_strata_wtp_mu_[k] = std_normal_rng();
  }
}

strata_wtp_mu_tau_ = std_normal_rng();

wtp_sigma = std_normal_rng(); 
// TODO Ed, need option to simulate data or condition on it
if (fit_wtp_model_to_data) { 
    reject("Need to write generative model for WTP data")
  int wtp_stratum_pos = 1;

  for (stratum_index in 1:num_strata) {
    int curr_wtp_stratum_size = wtp_strata_sizes[stratum_index];
    int wtp_stratum_end = wtp_stratum_pos + curr_wtp_stratum_size - 1;

    for (wtp_obs_index in wtp_stratum_pos:wtp_stratum_end) {
      if (wtp_response[wtp_obs_index] == -1) { // Decided to KEEP
        if (gift_choice[wtp_obs_index] == -1) { // Initial choice: BRACELET
          target += log(Phi_approx((- scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu_[stratum_index]) / wtp_sigma_));
        } else { // Initial choice: CALENDAR
          target += log(1 - Phi_approx((scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu_[stratum_index]) / wtp_sigma_));
        }
      } else { // Decided to SWITCH
        target += log(gift_choice[wtp_obs_index] *
          (Phi_approx((gift_choice[wtp_obs_index] * scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu_[stratum_index]) / wtp_sigma_) - Phi_approx(- strata_wtp_mu_[stratum_index] / wtp_sigma_)));
      }
    }
    wtp_stratum_pos = wtp_stratum_end + 1;
  }
}

// beliefs_parameters_sec.stan
row_vector[num_treatments] hyper_beta_1ord_;
row_vector[num_treatments] hyper_dist_beta_1ord_;
 
matrix[beliefs_use_stratum_level ? num_counties : 0, num_treatments] stratum_beta_1ord_raw_;
matrix[beliefs_use_cluster_level ? num_clusters : 0, num_treatments] cluster_beta_1ord_raw_;
matrix[beliefs_use_obs_level ? num_beliefs_obs : 0, num_treatments] obs_beta_1ord_raw_;
 
row_vector<lower = 0>[beliefs_use_stratum_level ? num_treatments : 0] stratum_beta_1ord_sd_;
row_vector<lower = 0>[beliefs_use_cluster_level ? num_treatments : 0] cluster_beta_1ord_sd_;
row_vector<lower = 0>[beliefs_use_obs_level ? num_treatments : 0] obs_beta_1ord_sd_;

matrix[beliefs_use_stratum_level ? num_counties : 0, num_treatments] stratum_dist_beta_1ord_raw_;
matrix[beliefs_use_cluster_level ? num_clusters : 0, num_treatments] cluster_dist_beta_1ord_raw_;
matrix[beliefs_use_obs_level ? num_beliefs_obs : 0, num_treatments] obs_dist_beta_1ord_raw_;

row_vector<lower = 0>[beliefs_use_stratum_level ? num_treatments : 0] stratum_dist_beta_1ord_sd_;
row_vector<lower = 0>[beliefs_use_cluster_level ? num_treatments : 0] cluster_dist_beta_1ord_sd_;
row_vector<lower = 0>[beliefs_use_obs_level ? num_treatments : 0] obs_dist_beta_1ord_sd_;
 
row_vector[num_treatments] hyper_beta_2ord_;
row_vector[num_treatments] hyper_dist_beta_2ord_;
 
matrix[beliefs_use_stratum_level? num_counties : 0, num_treatments] stratum_beta_2ord_raw_;
matrix[beliefs_use_cluster_level ? num_clusters : 0, num_treatments] cluster_beta_2ord_raw_;
matrix[beliefs_use_obs_level ? num_beliefs_obs : 0, num_treatments] obs_beta_2ord_raw_;
 
row_vector<lower = 0>[beliefs_use_stratum_level ? num_treatments : 0] stratum_beta_2ord_sd_;
row_vector<lower = 0>[beliefs_use_cluster_level ? num_treatments: 0] cluster_beta_2ord_sd_;
row_vector<lower = 0>[beliefs_use_obs_level ? num_treatments : 0] obs_beta_2ord_sd_;

matrix[beliefs_use_stratum_level? num_counties : 0, num_treatments] stratum_dist_beta_2ord_raw_;
matrix[beliefs_use_cluster_level ? num_clusters : 0, num_treatments] cluster_dist_beta_2ord_raw_;
matrix[beliefs_use_obs_level ? num_beliefs_obs : 0, num_treatments] obs_dist_beta_2ord_raw_;
 
row_vector<lower = 0>[beliefs_use_stratum_level ? num_treatments : 0] stratum_dist_beta_2ord_sd_;
row_vector<lower = 0>[beliefs_use_cluster_level ? num_treatments: 0] cluster_dist_beta_2ord_sd_;
row_vector<lower = 0>[beliefs_use_obs_level ? num_treatments : 0] obs_dist_beta_2ord_sd_;
 
vector[num_beliefs_obs] obs_beta_common_raw_;
real<lower = 0> obs_beta_common_sd_;


// simulate beliefs


// here ed
for (k in 1:num_beliefs_obs) {
    obs_beta_common_raw_[k] = std_normal_rng();
}
obs_beta_common_sd = normal_lb_rng(0, 0.125, 0);

hyper_beta_1ord_[1] = std_normal_rng();
hyper_beta_1ord[2:] = normal_rng(0, 0.5);

hyper_dist_beta_1ord[1] = std_normal_rng();
hyper_dist_beta_1ord[2:] = normal_rng(0, 0.5);

if (beliefs_use_obs_level) { 
  for (k in 1:num_treatments){
    obs_beta_1ord_raw_[, k] = normal_rng(0, 1);
    obs_beta_1ord_sd_[k] = normal_rng(0, 0.125);
    obs_dist_beta_1ord_raw_[, k] = normal_rng(0, 1);
    obs_dist_beta_1ord_sd_[k] = normal_rng(0, 0.125);
  }
  
}

if (beliefs_use_cluster_level) {
  to_vector(cluster_beta_1ord_raw) ~ std_normal();
  cluster_beta_1ord_sd ~ normal(0, 0.25);
  
  to_vector(cluster_dist_beta_1ord_raw) ~ std_normal();
  cluster_dist_beta_1ord_sd ~ normal(0, 0.25);
}

if (beliefs_use_stratum_level) {
  to_vector(stratum_beta_1ord_raw) ~ std_normal();
  stratum_beta_1ord_sd ~ normal(0, 0.5);
  
  to_vector(stratum_dist_beta_1ord_raw) ~ std_normal();
  stratum_dist_beta_1ord_sd ~ normal(0, 0.5);
}

if (fit_beliefs_model_to_data) {
  num_knows_1ord ~ binomial_logit(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_1ord, centered_obs_dist_beta_1ord, cluster_standard_dist[beliefs_cluster_index])
  ); 
}

hyper_beta_2ord[1] ~ normal(0, 1);
hyper_beta_2ord[2:] ~ normal(0, 0.5);

hyper_dist_beta_2ord[1] ~ normal(0, 1);
hyper_dist_beta_2ord[2:] ~ normal(0, 0.5);

if (beliefs_use_obs_level) { 
  to_vector(obs_beta_2ord_raw) ~ std_normal();
  obs_beta_2ord_sd ~ normal(0, 0.125);
  
  to_vector(obs_dist_beta_2ord_raw) ~ std_normal();
  obs_dist_beta_2ord_sd ~ normal(0, 0.125);
}

if (beliefs_use_cluster_level) {
  to_vector(cluster_beta_2ord_raw) ~ std_normal();
  cluster_beta_2ord_sd ~ normal(0, 0.25);
  
  to_vector(cluster_dist_beta_2ord_raw) ~ std_normal();
  cluster_dist_beta_2ord_sd ~ normal(0, 0.25);
}

if (beliefs_use_stratum_level) {
  to_vector(stratum_beta_2ord_raw) ~ std_normal();
  stratum_beta_2ord_sd ~ normal(0, 0.5);
  
  to_vector(stratum_dist_beta_2ord_raw) ~ std_normal();
  stratum_dist_beta_2ord_sd ~ normal(0, 0.5);
}

if (fit_beliefs_model_to_data) {
  num_knows_2ord ~ binomial_logit(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_2ord, centered_obs_dist_beta_2ord, cluster_standard_dist[beliefs_cluster_index])
  );
}

// here ed

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

// Semiparameteric Cost Model

matrix[num_treatments_semiparam, num_knots_v] u_splines_v_raw_;
real<lower = 0> u_splines_v_sigma_;

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

// wtp_transformed_parameters.stan
vector[num_strata] strata_effect_wtp_mu_ = use_strata_levels ? raw_strata_wtp_mu_ * strata_wtp_mu_tau_ : rep_vector(0, num_strata);
vector[num_strata] strata_wtp_mu_ = hyper_wtp_mu_ + strata_effect_wtp_mu_;

// beliefs_transformed_parameters.stan
matrix[num_beliefs_obs, num_treatments] centered_obs_beta_1ord_; 
matrix[num_beliefs_obs, num_treatments] centered_obs_beta_2ord_;  

matrix[num_clusters, num_treatments] centered_cluster_beta_1ord_; 
matrix[num_clusters, num_treatments] centered_cluster_beta_2ord_;  

matrix[num_beliefs_obs, num_treatments] centered_obs_dist_beta_1ord_; 
matrix[num_beliefs_obs, num_treatments] centered_obs_dist_beta_2ord_;  

matrix[num_clusters, num_treatments] centered_cluster_dist_beta_1ord_; 
matrix[num_clusters, num_treatments] centered_cluster_dist_beta_2ord_;  
{
  matrix[num_counties, num_treatments] stratum_beta_1ord_;
  matrix[num_clusters, num_treatments] cluster_beta_1ord_;
  matrix[num_beliefs_obs, num_treatments] obs_beta_1ord_;
  
  matrix[num_counties, num_treatments] stratum_beta_2ord_;
  matrix[num_clusters, num_treatments] cluster_beta_2ord_;
  matrix[num_beliefs_obs, num_treatments] obs_beta_2ord_;
  
  matrix[num_counties, num_treatments] stratum_dist_beta_1ord_;
  matrix[num_clusters, num_treatments] cluster_dist_beta_1ord_;
  matrix[num_beliefs_obs, num_treatments] obs_dist_beta_1ord_;
  
  matrix[num_counties, num_treatments] stratum_dist_beta_2ord_;
  matrix[num_clusters, num_treatments] cluster_dist_beta_2ord_;
  matrix[num_beliefs_obs, num_treatments] obs_dist_beta_2ord_;
  
  vector[num_beliefs_obs] obs_beta_common_ = obs_beta_common_raw_ * obs_beta_common_sd_;
  
  stratum_beta_1ord_ = beliefs_use_stratum_level ? stratum_beta_1ord_raw_ .* rep_matrix(stratum_beta_1ord_sd_, num_counties) : rep_matrix(0, num_counties, num_treatments);
  cluster_beta_1ord_ = beliefs_use_cluster_level ? cluster_beta_1ord_raw_ .* rep_matrix(cluster_beta_1ord_sd_, num_clusters) : rep_matrix(0, num_clusters, num_treatments);
  obs_beta_1ord_ = beliefs_use_obs_level ? obs_beta_1ord_raw_ .* rep_matrix(obs_beta_1ord_sd_, num_beliefs_obs) : rep_matrix(0, num_beliefs_obs, num_treatments);
  
  stratum_dist_beta_1ord_ = beliefs_use_stratum_level ? stratum_dist_beta_1ord_raw_ .* rep_matrix(stratum_dist_beta_1ord_sd_, num_counties) : rep_matrix(0, num_counties, num_treatments);
  cluster_dist_beta_1ord_ = beliefs_use_cluster_level ? cluster_dist_beta_1ord_raw_ .* rep_matrix(cluster_dist_beta_1ord_sd_, num_clusters) : rep_matrix(0, num_clusters, num_treatments);
  obs_dist_beta_1ord_ = beliefs_use_obs_level ? obs_dist_beta_1ord_raw_ .* rep_matrix(obs_dist_beta_1ord_sd_, num_beliefs_obs) : rep_matrix(0, num_beliefs_obs, num_treatments);
  
  stratum_beta_2ord_ = beliefs_use_stratum_level ? stratum_beta_2ord_raw_ .* rep_matrix(stratum_beta_2ord_sd_, num_counties) : rep_matrix(0, num_counties, num_treatments);
  cluster_beta_2ord_ = beliefs_use_cluster_level ? cluster_beta_2ord_raw_ .* rep_matrix(cluster_beta_2ord_sd_, num_clusters) : rep_matrix(0, num_clusters, num_treatments);
  obs_beta_2ord_ = beliefs_use_obs_level ? obs_beta_2ord_raw_ .* rep_matrix(obs_beta_2ord_sd_, num_beliefs_obs) : rep_matrix(0, num_beliefs_obs, num_treatments);
  
  stratum_dist_beta_2ord_ = beliefs_use_stratum_level ? stratum_dist_beta_2ord_raw_ .* rep_matrix(stratum_dist_beta_2ord_sd_, num_counties) : rep_matrix(0, num_counties, num_treatments);
  cluster_dist_beta_2ord_ = beliefs_use_cluster_level ? cluster_dist_beta_2ord_raw_ .* rep_matrix(cluster_dist_beta_2ord_sd_, num_clusters) : rep_matrix(0, num_clusters, num_treatments);
  obs_dist_beta_2ord_ = beliefs_use_obs_level ? obs_dist_beta_2ord_raw_ .* rep_matrix(obs_dist_beta_2ord_sd_, num_beliefs_obs) : rep_matrix(0, num_beliefs_obs, num_treatments);
  
  centered_cluster_beta_1ord_ = rep_matrix(hyper_beta_1ord_, num_clusters) + stratum_beta_1ord_[cluster_county_id] + cluster_beta_1ord_;
  centered_obs_beta_1ord_ = centered_cluster_beta_1ord_[beliefs_cluster_index] + obs_beta_1ord_ + rep_matrix(obs_beta_common_, num_treatments); 
  
  centered_cluster_dist_beta_1ord_ = rep_matrix(hyper_dist_beta_1ord_, num_clusters) + stratum_dist_beta_1ord_[cluster_county_id] + cluster_dist_beta_1ord_;
  centered_obs_dist_beta_1ord_ = centered_cluster_dist_beta_1ord_[beliefs_cluster_index] + obs_dist_beta_1ord_;
  
  centered_cluster_beta_2ord_ = rep_matrix(hyper_beta_2ord_, num_clusters) + stratum_beta_2ord_[cluster_county_id] + cluster_beta_2ord_;
  centered_obs_beta_2ord_ = centered_cluster_beta_2ord_[beliefs_cluster_index] + obs_beta_2ord_ + rep_matrix(obs_beta_common_, num_treatments); 
  
  centered_cluster_dist_beta_2ord_ = rep_matrix(hyper_dist_beta_2ord_, num_clusters) + stratum_dist_beta_2ord_[cluster_county_id] + cluster_dist_beta_2ord_;
  centered_obs_dist_beta_2ord_ = centered_cluster_dist_beta_2ord_[beliefs_cluster_index] + obs_dist_beta_2ord_;
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
    u_sd = rep_vector(use_wtp_model ? sqrt(square(raw_u_sd_[1]) + square(wtp_sigma_ * wtp_value_utility_)) : raw_u_sd_[1], num_treatments);  
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