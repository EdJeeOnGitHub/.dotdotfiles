
// dist_parameters_sec.stan
array[num_discrete_dist] simplex[num_dist_group_mix] group_dist_mix_;

array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_raw_;
array[num_discrete_dist] vector<lower = 0>[num_dist_group_mix] group_dist_sd_;

matrix[use_dist_county_effects ? num_discrete_dist : 0, num_counties] county_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] county_dist_effect_sd_;

matrix[use_dist_cluster_effects ? num_discrete_dist : 0, num_clusters] cluster_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] cluster_dist_effect_sd_;

// dist draw from priors

if (use_dist_county_effects) {
  for (k in 1:num_discrete_dist)
  county_dist_effect_raw_[k,] = normal_rng(0, 1);
  county_dist_effect_sd_[k] = normal_lb_rng(0, county_dist_effect_sd_sd, 0);
  cluster_dist_effect_raw_[k,] = normal_rng(0, 1);
  cluster_dist_effect_sd_[k] = normal_lb_rng(0, cluster_dist_effect_sd_sd, 0);
}

if (use_dist_cluster_effects) {
  to_vector(cluster_dist_effect_raw) ~ std_normal();
  cluster_dist_effect_sd ~ normal(0, cluster_dist_effect_sd_sd);
}

for (dist_group_index in 1:num_discrete_dist) {
  
  for (dist_group_mix_index in 1:num_dist_group_mix) {
    if (lognormal_dist_model) {
      // group_dist_mean[dist_group_index, dist_group_mix_index] ~ normal(hyper_dist_mean_mean, hyper_dist_mean_sd);
      group_dist_mean_raw[dist_group_index, dist_group_mix_index] ~ std_normal();
    } else {
      group_dist_mean_raw[dist_group_index, dist_group_mix_index] ~ std_normal() T[0, ];
      // group_dist_mean[dist_group_index, dist_group_mix_index] ~ normal(hyper_dist_mean_mean, hyper_dist_mean_sd) T[0, ];
    }
  }
  
  group_dist_sd[dist_group_index] ~ normal(0, hyper_dist_sd_sd);
}



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

