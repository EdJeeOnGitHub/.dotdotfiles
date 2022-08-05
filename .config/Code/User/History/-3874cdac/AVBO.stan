obs_beta_common_raw ~ std_normal();
obs_beta_common_sd ~ normal(0, 0.125);

hyper_beta_1ord[1] ~ normal(0, 1);
hyper_beta_1ord[2:] ~ normal(0, 0.5);

hyper_dist_beta_1ord[1] ~ normal(0, 1);
hyper_dist_beta_1ord[2:] ~ normal(0, 0.5);

if (beliefs_use_obs_level) { 
  to_vector(obs_beta_1ord_raw) ~ std_normal();
  obs_beta_1ord_sd ~ normal(0, 0.125);
  
  to_vector(obs_dist_beta_1ord_raw) ~ std_normal();
  obs_dist_beta_1ord_sd ~ normal(0, 0.125);
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

if (fit_beliefs_model_to_data && !sbc) {
  num_knows_1ord ~ binomial_logit(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_1ord, centered_obs_dist_beta_1ord, cluster_standard_dist[beliefs_cluster_index])
  ); 
}
if (sbc) {
  sim_num_knows_1ord ~ binomial_logit(
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

if (fit_beliefs_model_to_data && !sbc) {
  num_knows_2ord ~ binomial_logit(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_2ord, centered_obs_dist_beta_2ord, cluster_standard_dist[beliefs_cluster_index])
  );
}
if (sbc) {
  sim_num_knows_2ord ~ binomial_logit(
    num_recognized, 
    calculate_beliefs_latent_predictor(beliefs_treatment_design_matrix, centered_obs_beta_2ord, centered_obs_dist_beta_2ord, cluster_standard_dist[beliefs_cluster_index])
  );
}