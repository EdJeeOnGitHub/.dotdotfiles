
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
    obs_beta_1ord_raw_[, k] = normal_rng(0, 1);
    obs_beta_1ord_sd_[k] = normal_lb_rng(0, 0.125, 0);
    obs_dist_beta_1ord_raw_[, k] = normal_rng(0, 1);
    obs_dist_beta_1ord_sd_[k] = normal_lb_rng(0, 0.125, 0);

    obs_beta_2ord_raw_[, k] = normal_rng(0, 1);
    obs_beta_2ord_sd_[k] = normal_lb_rng(0, 0.125, 0);
    obs_dist_beta_2ord_raw_[, k] = normal_rng(0, 1);
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

