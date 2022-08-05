
functions {
#include util.stan

  real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb | mu, sigma);  // cdf for bounds
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
  }
}

data {
#include base_data_sec.stan
#include takeup_data_sec.stan

  int<lower = 0, upper = 1> use_age_group_gp;

  real<lower = 0> reduced_beta_county_sd_sd;
  real<lower = 0> reduced_beta_cluster_sd_sd;
  
  real<lower = 0> beta_far_effect_sd;
  real<lower = 0> beta_far_ink_effect_sd;
  real<lower = 0> beta_far_calendar_effect_sd; 
  real<lower = 0> beta_far_bracelet_effect_sd;
  
  vector<lower = 0>[num_treatments * num_discrete_dist] age_group_alpha_sd;
  vector<lower = 0>[num_treatments * num_discrete_dist] age_group_rho_sd;
  int<lower = 0, upper = 1> sbc;
}

transformed data {
#include base_transformed_data.stan 
#include takeup_transformed_data.stan
  array[num_age_groups] int<lower = 1, upper = num_age_groups> age_groups_dist = seq_len(num_age_groups);
#include sbc_reduced_data.stan

}

parameters {
  // Levels: control ink calendar bracelet
  vector[num_discrete_dist] beta_control;
  vector[num_discrete_dist] beta_ink_effect;
  vector[num_discrete_dist] beta_calendar_effect;
  vector[num_discrete_dist] beta_bracelet_effect;
  
  vector[use_cluster_effects ? num_clusters : 0] reduced_beta_cluster_raw;
  real<lower = 0> reduced_beta_cluster_sd;
  
  matrix[use_county_effects ? num_counties : 0, num_dist_group_treatments] reduced_beta_county_raw;
  row_vector<lower = 0>[use_county_effects ? num_dist_group_treatments : 0] reduced_beta_county_sd;
  
  matrix[use_age_groups ? num_age_groups : 0, num_dist_group_treatments] reduced_beta_age_group_raw;
  row_vector<lower = 0>[use_age_groups ? num_dist_group_treatments : 0] reduced_beta_age_group_alpha;
  row_vector<lower = 0>[use_age_groups && use_age_group_gp ? num_dist_group_treatments : 0] reduced_beta_age_group_rho;

}

transformed parameters {
  vector[num_dist_group_treatments] beta; 
  vector[num_dist_group_treatments] reduced_treatment_effect;
  // matrix[num_clusters, num_age_groups] reduced_cluster_benefit_cost;
  vector[num_clusters] reduced_cluster_benefit_cost;
  vector[num_clusters] reduced_beta_cluster = rep_vector(0, num_clusters);
  matrix[num_counties, num_dist_group_treatments] reduced_beta_county = rep_matrix(0, num_counties, num_dist_group_treatments);
  matrix[num_age_groups, num_dist_group_treatments] reduced_beta_age_group = rep_matrix(0, num_age_groups, num_dist_group_treatments);
  
  matrix<lower = 0, upper = 1>[num_clusters, num_age_groups] reduced_cluster_takeup_prob;
  
  for (dist_index in 1:num_discrete_dist) {
    beta[(num_treatments * (dist_index - 1) + 1):(num_treatments * dist_index)] = 
      [ beta_control[dist_index], beta_ink_effect[dist_index], beta_calendar_effect[dist_index], beta_bracelet_effect[dist_index] ]';
  }
 
  reduced_treatment_effect = treatment_map_design_matrix * beta;
  // reduced_cluster_benefit_cost = rep_matrix(reduced_treatment_effect[cluster_assigned_dist_group_treatment], num_age_groups);
  reduced_cluster_benefit_cost = reduced_treatment_effect[cluster_assigned_dist_group_treatment];
  
  if (use_cluster_effects) {
    reduced_beta_cluster = reduced_beta_cluster_raw * reduced_beta_cluster_sd;
    
    // reduced_cluster_benefit_cost += rep_matrix(reduced_beta_cluster, num_age_groups);
    reduced_cluster_benefit_cost += reduced_beta_cluster;
  }
  
  if (use_county_effects) {
    vector[num_clusters] county_effects;
    
    reduced_beta_county = reduced_beta_county_raw .* rep_matrix(reduced_beta_county_sd, num_counties);
    
    county_effects = rows_dot_product(cluster_treatment_design_matrix, reduced_beta_county[cluster_county_id]); 
    // reduced_cluster_benefit_cost += rep_matrix(county_effects, num_age_groups);
    reduced_cluster_benefit_cost += county_effects;
  }
  
  if (use_age_groups) {
    if (use_age_group_gp) { 
      for (treatment_index in 1:num_dist_group_treatments) {
        reduced_beta_age_group[, treatment_index] = calc_gp_trend(age_groups_dist, reduced_beta_age_group_alpha[treatment_index], reduced_beta_age_group_rho[treatment_index], reduced_beta_age_group_raw[, treatment_index]);
      }
    } else {
      reduced_beta_age_group = reduced_beta_age_group_raw .* rep_matrix(reduced_beta_age_group_alpha, num_age_groups);
    }
  }
  
  for (age_group_index in 1:num_age_groups) {
    reduced_cluster_takeup_prob[, age_group_index] = Phi_approx(reduced_cluster_benefit_cost + cluster_treatment_design_matrix * reduced_beta_age_group[age_group_index]');
  }
}

model {
  beta_control ~ normal(0, [ beta_control_sd, beta_far_effect_sd ]');
  beta_ink_effect ~ normal(0, [ beta_ink_effect_sd, beta_far_ink_effect_sd ]');
  beta_calendar_effect ~ normal(0, [ beta_calendar_effect_sd, beta_far_calendar_effect_sd ]');
  beta_bracelet_effect ~ normal(0, [ beta_bracelet_effect_sd, beta_far_bracelet_effect_sd ]');
  
  if (use_cluster_effects) {
    reduced_beta_cluster_raw ~ std_normal();
  }
  
  reduced_beta_cluster_sd ~ normal(0, reduced_beta_cluster_sd_sd);
  
  if (use_county_effects) {
    to_vector(reduced_beta_county_raw) ~ std_normal();
    reduced_beta_county_sd ~ normal(0, reduced_beta_county_sd_sd);
  }
  
  if (use_age_groups) {
    to_vector(reduced_beta_age_group_raw) ~ std_normal();
    reduced_beta_age_group_alpha ~ normal(0, age_group_alpha_sd);
    
    if (use_age_group_gp) {
      reduced_beta_age_group_rho ~ normal(0, age_group_rho_sd);
    }
  }
  profile("model fitting") {
    if (fit_model_to_data) {
      // Take-up Likelihood 
      
      if (use_binomial) {
        if (sbc == 1) {
          // not yet implemented
        } else {
          for (age_group_index in 1:num_age_groups) {
            cluster_takeup_count[included_clusters, age_group_index] ~ binomial(
              cluster_size[included_clusters, age_group_index], 
              // reduced_cluster_takeup_prob[included_clusters]
              Phi_approx(reduced_cluster_benefit_cost[included_clusters] + cluster_treatment_design_matrix[included_clusters] * reduced_beta_age_group[age_group_index]')
            );
          }
        }
      } else {
        // takeup[included_monitored_obs] ~ bernoulli(reduced_cluster_takeup_prob[obs_cluster_id[included_monitored_obs]]);
        if (sbc == 1) {
          sim_takeup[included_monitored_obs] ~ bernoulli(
            Phi_approx(
              reduced_cluster_benefit_cost[obs_cluster_id[included_monitored_obs]] + rows_dot_product(cluster_treatment_design_matrix[obs_cluster_id[included_monitored_obs]], reduced_beta_age_group[obs_age_group[included_monitored_obs]])
            )
          );
        } else {
          takeup[included_monitored_obs] ~ bernoulli(
            Phi_approx(
              reduced_cluster_benefit_cost[obs_cluster_id[included_monitored_obs]] + rows_dot_product(cluster_treatment_design_matrix[obs_cluster_id[included_monitored_obs]], reduced_beta_age_group[obs_age_group[included_monitored_obs]])
            )
          );

        }
      }
    }
  }
}

generated quantities { 
  array[num_dist_group_treatments] int ranks_;
   
  array[num_dist_group_treatments] matrix[num_clusters, num_age_groups] cluster_age_group_cf_benefit_cost; 
  array[num_dist_group_treatments, 1] matrix[num_clusters, num_age_groups] cluster_age_group_cf_cutoff; 
  
  array[num_dist_group_treatments] vector[num_clusters] cluster_cf_benefit_cost; 
  array[num_dist_group_treatments, 1] vector[num_clusters] cluster_cf_cutoff; 
  
  matrix[generate_rep ? num_clusters : 0, num_age_groups] cluster_age_group_rep_benefit_cost; 
  vector[generate_rep ? num_clusters : 0] cluster_rep_benefit_cost; 
  
  // Cross Validation
  vector[cross_validate ? (use_binomial || cluster_log_lik ? num_included_clusters : num_included_obs) : 0] log_lik;
  vector[cross_validate ? (use_binomial || cluster_log_lik ? num_excluded_clusters : num_excluded_obs) : 0] log_lik_heldout;
  
  profile("rank generation") {
    for (k in 1:num_dist_group_treatments) {
      if (sbc == 1){
        ranks_[k] = beta[k] > beta_[k];
      } else {
        ranks_[k] = -1;
      }
    }
  }
    
  for (treatment_index in 1:num_dist_group_treatments) {
    cluster_age_group_cf_benefit_cost[treatment_index] =
      rep_matrix(reduced_beta_cluster, num_age_groups)
      + rep_matrix((reduced_beta_county * treatment_map_design_matrix[treatment_index]')[cluster_county_id], num_age_groups)
      + rep_matrix(reduced_treatment_effect[treatment_index], num_clusters, num_age_groups) 
      + rep_matrix((treatment_map_design_matrix[treatment_index] * reduced_beta_age_group'), num_clusters);
      
    cluster_age_group_cf_cutoff[treatment_index, 1] = - cluster_age_group_cf_benefit_cost[treatment_index]; 
    
    cluster_cf_benefit_cost[treatment_index] = rows_dot_product(cluster_age_group_cf_benefit_cost[treatment_index], cluster_age_group_prop); 
    cluster_cf_cutoff[treatment_index, 1] = rows_dot_product(cluster_age_group_cf_cutoff[treatment_index, 1], cluster_age_group_prop);
  }
  
  // Cross Validation 
  if (cross_validate) { 
    log_lik = rep_vector(negative_infinity(), use_binomial || cluster_log_lik ? num_included_clusters : num_included_obs); 
    log_lik_heldout = rep_vector(negative_infinity(), use_binomial || cluster_log_lik ? num_excluded_clusters : num_excluded_obs); 
    
    if (use_binomial) {
      for (cluster_index_index in 1:num_included_clusters) {
        int cluster_index = included_clusters[cluster_index_index];
        
        log_lik[cluster_index_index] = 0;
        
        for (age_group_index in 1:num_age_groups) {
          log_lik[cluster_index_index] += binomial_lpmf(cluster_takeup_count[cluster_index, age_group_index] | cluster_size[cluster_index, age_group_index], reduced_cluster_takeup_prob[cluster_index, age_group_index]);
        }
      }
      
      for (cluster_index_index in 1:num_excluded_clusters) {
        int cluster_index = excluded_clusters[cluster_index_index];
        
        log_lik_heldout[cluster_index_index] = 0;
        
        for (age_group_index in 1:num_age_groups) {
          log_lik_heldout[cluster_index_index] += binomial_lpmf(cluster_takeup_count[cluster_index, age_group_index] | cluster_size[cluster_index, age_group_index], reduced_cluster_takeup_prob[cluster_index, age_group_index]);
        }
      }
    } else {
      vector[num_clusters] temp_log_lik = rep_vector(0, num_clusters);
      
      for (obs_index_index in 1:num_included_monitored_obs) {
        int obs_index = included_monitored_obs[obs_index_index];
        int cluster_index = obs_cluster_id[obs_index];
        int curr_age_group = obs_age_group[obs_index];
        
        real curr_log_lik = bernoulli_lpmf(takeup[obs_index] | reduced_cluster_takeup_prob[cluster_index:cluster_index, curr_age_group]);
        
        if (cluster_log_lik) {
          temp_log_lik[cluster_index] += curr_log_lik;
        } else {
          log_lik[obs_index_index] = curr_log_lik;
        }
      }
      
      for (obs_index_index in 1:num_excluded_monitored_obs) {
        int obs_index = excluded_monitored_obs[obs_index_index];
        int cluster_index = obs_cluster_id[obs_index];
        int curr_age_group = obs_age_group[obs_index];
        
        real  curr_log_lik = bernoulli_lpmf(takeup[obs_index] | reduced_cluster_takeup_prob[cluster_index:cluster_index, curr_age_group]);
        
        if (cluster_log_lik) {
          temp_log_lik[cluster_index] += curr_log_lik;
        } else {
          log_lik_heldout[obs_index_index] = curr_log_lik;
        }
      }
      
      if (cluster_log_lik) {
        log_lik = temp_log_lik[included_clusters];
        log_lik_heldout = temp_log_lik[excluded_clusters];
      }
    }
  }
  
  if (generate_rep) {
    for (cluster_index in 1:num_clusters) {
      real rep_beta_cluster = use_cluster_effects ? normal_rng(0, reduced_beta_cluster_sd) : 0;
      vector[num_dist_group_treatments] rep_beta_county = use_county_effects ? to_vector(normal_rng(rep_array(0, num_dist_group_treatments), reduced_beta_county_sd')) : rep_vector(0, num_dist_group_treatments);
      
      cluster_age_group_rep_benefit_cost[cluster_index] = 
        reduced_treatment_effect[cluster_assigned_dist_group_treatment[cluster_index]]
        + rep_beta_cluster 
        + treatment_map_design_matrix[cluster_assigned_dist_group_treatment[cluster_index]] * rep_beta_county
        + treatment_map_design_matrix[cluster_assigned_dist_group_treatment[cluster_index]] * reduced_beta_age_group';
        
      cluster_rep_benefit_cost[cluster_index] = cluster_age_group_rep_benefit_cost[cluster_index] * cluster_age_group_prop[cluster_index]';
      // cluster_rep_benefit_cost[cluster_index] = reduced_treatment_effect[cluster_assigned_dist_group_treatment[cluster_index]]
      //   + rep_beta_cluster + treatment_map_design_matrix[cluster_assigned_dist_group_treatment[cluster_index]] * rep_beta_county;
    }
  }
}
