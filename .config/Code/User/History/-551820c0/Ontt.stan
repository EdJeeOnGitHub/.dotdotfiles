  // SBC
  vector[num_discrete_dist] beta_control_;
  vector[num_discrete_dist] beta_ink_effect_;
  vector[num_discrete_dist] beta_calendar_effect_;
  vector[num_discrete_dist] beta_bracelet_effect_;
  
  vector[use_cluster_effects ? num_clusters : 0] reduced_beta_cluster_raw_;
  real<lower = 0> reduced_beta_cluster_sd_;
  
  matrix[use_county_effects ? num_counties : 0, num_dist_group_treatments] reduced_beta_county_raw_;
  row_vector<lower = 0>[use_county_effects ? num_dist_group_treatments : 0] reduced_beta_county_sd_;
  
  matrix[use_age_groups ? num_age_groups : 0, num_dist_group_treatments] reduced_beta_age_group_raw_;
  row_vector<lower = 0>[use_age_groups ? num_dist_group_treatments : 0] reduced_beta_age_group_alpha_;
  row_vector<lower = 0>[use_age_groups && use_age_group_gp ? num_dist_group_treatments : 0] reduced_beta_age_group_rho_;
