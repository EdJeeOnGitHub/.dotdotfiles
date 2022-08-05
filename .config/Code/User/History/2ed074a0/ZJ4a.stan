
// dist_parameters_sec.stan
array[num_discrete_dist] simplex[num_dist_group_mix] group_dist_mix_;

array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_raw_;
array[num_discrete_dist] vector<lower = 0>[num_dist_group_mix] group_dist_sd_;

matrix[use_dist_county_effects ? num_discrete_dist : 0, num_counties] county_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] county_dist_effect_sd_;

matrix[use_dist_cluster_effects ? num_discrete_dist : 0, num_clusters] cluster_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] cluster_dist_effect_sd_;
vector[num_clusters] sim_cluster_standard_dist; // Standardized distance to treatment

// dist draw from priors

for (k in 1:num_discrete_dist) {
  if (use_dist_county_effects) {
    county_dist_effect_raw_[k,] = to_row_vector(normal_rng(rep_array(0, num_counties), 1));
    county_dist_effect_sd_[k] = normal_lb_rng(0, county_dist_effect_sd_sd, 0);
    }

  if (use_dist_cluster_effects) {
    cluster_dist_effect_raw_[k,] = to_row_vector(normal_rng(rep_array(0, num_clusters), 1));
    cluster_dist_effect_sd_[k] = normal_lb_rng(0, cluster_dist_effect_sd_sd, 0);
  }

}


for (dist_group_index in 1:num_discrete_dist) {
  
  for (dist_group_mix_index in 1:num_dist_group_mix) {
    if (lognormal_dist_model) {
      // group_dist_mean[dist_group_index, dist_group_mix_index] ~ normal(hyper_dist_mean_mean, hyper_dist_mean_sd);
      array[num_dist_group_mix] real tmp_group_dist_mean_raw_;
      tmp_group_dist_mean_raw_ = normal_rng(rep_array(0, num_dist_group_mix), 1);
      group_dist_mean_raw_[dist_group_index, :] = to_vector(sort_asc(tmp_group_dist_mean_raw_));
    } else {
      array[num_dist_group_mix] real tmp_group_dist_mean_raw_;
      for (k in 1:num_dist_group_mix) {
        tmp_group_dist_mean_raw_[k] = normal_lb_rng(0, 1, 0);
      }
      group_dist_mean_raw_[dist_group_index, :] = to_vector(sort_asc(tmp_group_dist_mean_raw_));
      // group_dist_mean[dist_group_index, dist_group_mix_index] ~ normal(hyper_dist_mean_mean, hyper_dist_mean_sd) T[0, ];
    }

    group_dist_sd_[dist_group_index, dist_group_mix_index] = normal_lb_rng(0, hyper_dist_sd_sd, 0);
  }
  
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


// dist generate outcomes
if (num_dist_group_mix > 1) {
  reject("Only implemented for M = 1.");
}
for (k in 1:num_discrete_dist) {
  group_dist_mix_[k] = 1;
}



for (cluster_index in 1:num_clusters) {
  int curr_assigned_dist_group = cluster_treatment_map[cluster_assigned_dist_group_treatment[cluster_index], 2];
  
  if (fit_dist_model_to_data) {
    if (lognormal_dist_model) {
      sim_cluster_standard_dist[cluster_index] = lognormal_rng(
        group_dist_mean[curr_assigned_dist_group, 1] 
        + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]]
        + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
        group_dist_sd[curr_assigned_dist_group, 1]
      );
    } else {
      sim_cluster_standard_dist[cluster_index] = normal_lb_rng(
        group_dist_mean[curr_assigned_dist_group, 1] 
        + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]]
        + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
        group_dist_sd[curr_assigned_dist_group, 1],
        0
      );

    }
    
  }
}