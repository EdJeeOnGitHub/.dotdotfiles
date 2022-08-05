
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

array[num_beliefs_obs] int<lower = 0, upper = know_table_A_sample_size> sim_num_knows_1ord;
array[num_beliefs_obs] int<lower = 0, upper = know_table_A_sample_size> sim_num_knows_2ord;


// simulate beliefs
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
    obs_beta_1ord_raw_[, k] = to_vector(normal_rng(rep_array(0, num_beliefs_obs), 1));
    obs_beta_1ord_sd_[k] = normal_lb_rng(0, 0.125, 0);
    obs_dist_beta_1ord_raw_[, k] = to_vector(normal_rng(rep_array(0, num_beliefs_obs), 1));
    obs_dist_beta_1ord_sd_[k] = normal_lb_rng(0, 0.125, 0);

    obs_beta_2ord_raw_[, k] = to_vector(normal_rng(rep_array(0, num_beliefs_obs), 1));
    obs_beta_2ord_sd_[k] = normal_lb_rng(0, 0.125, 0);
    obs_dist_beta_2ord_raw_[, k] = to_vector(normal_rng(rep_array(0, num_beliefs_obs), 1));
    obs_dist_beta_2ord_sd_[k] = normal_lb_rng(0, 0.125, 0);
  }
}

if (beliefs_use_cluster_level) {
  for (k in 1:num_treatments) {
    cluster_beta_1ord_raw_[, k] = normal_rng(0, 1); 
    cluster_beta_1ord_sd_[k] = normal_lb_rng(0, 0.25, 0);

    cluster_dist_beta_1ord_raw_[, k] = normal_rng(0, 1);
    cluster_dist_beta_1ord_sd_[k] = normal_lb_rng(0, 0.25, 0);

    cluster_beta_2ord_raw_[, k] = normal_rng(0, 1); 
    cluster_beta_2ord_sd_[k] = normal_lb_rng(0, 0.25, 0);

    cluster_dist_beta_2ord_raw_[, k] = normal_rng(0, 1);
    cluster_dist_beta_2ord_sd_[k] = normal_lb_rng(0, 0.25, 0);
  }
  
}

if (beliefs_use_stratum_level) {
  stratum_beta_1ord_raw_[, k] = normal_rng(0, 1);
  stratum_beta_1ord_sd_k_[k] = normal_lb_rng(0, 0.5, 0);
  
  stratum_dist_beta_1ord_raw_[, k] = normal_rng(0, 1);
  stratum_dist_beta_1ord_sd_[k] = normal_lb_rng(0, 0.5, 0);

  stratum_beta_2ord_raw_[, k] = normal_rng(0, 1);
  stratum_beta_2ord_sd_k_[k] = normal_lb_rng(0, 0.5, 0);
  
  stratum_dist_beta_2ord_raw_[, k] = normal_rng(0, 1);
  stratum_dist_beta_2ord_sd_[k] = normal_lb_rng(0, 0.5, 0);
}


hyper_beta_2ord_[1] = std_normal_rng();
hyper_beta_2ord_[2:] = normal_rng(0, 0.5);

hyper_dist_beta_2ord_[1] = std_normal_rng();
hyper_dist_beta_2ord_[2:] = normal_rng(0, 0.5);


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

if (fit_beliefs_model_to_data) {
sim_num_knows_1ord = binomial_logit_rng(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_1ord_, centered_obs_dist_beta_1ord_, cluster_standard_dist[beliefs_cluster_index])
); 
}

if (fit_beliefs_model_to_data) {
sim_num_knows_2ord = binomial_logit_rng(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_2ord_, centered_obs_dist_beta_2ord_, cluster_standard_dist[beliefs_cluster_index])
);
}
