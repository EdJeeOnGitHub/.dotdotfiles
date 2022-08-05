if (use_dist_county_effects) {
  to_vector(county_dist_effect_raw) ~ std_normal();
  county_dist_effect_sd ~ normal(0, county_dist_effect_sd_sd);
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

for (cluster_index in 1:num_clusters) {
  int curr_assigned_dist_group = cluster_treatment_map[cluster_assigned_dist_group_treatment[cluster_index], 2];
  
  if (fit_dist_model_to_data && !sbc) {
    vector[num_dist_group_mix] group_dist_mix_lp;
    
    for (group_dist_mix_index in 1:num_dist_group_mix) {
      if (lognormal_dist_model) {
        group_dist_mix_lp[group_dist_mix_index] =
          lognormal_lpdf(cluster_standard_dist[cluster_index]  | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index] 
                                                                + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]] 
                                                                + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
                                                                group_dist_sd[curr_assigned_dist_group, group_dist_mix_index])
          + log(group_dist_mix[curr_assigned_dist_group, group_dist_mix_index]); 
      } else { // think this is wrong? 
        group_dist_mix_lp[group_dist_mix_index] = 
          normal_lpdf(cluster_standard_dist[cluster_index] | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index] 
                                                             + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]] 
                                                             + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
                                                             group_dist_sd[curr_assigned_dist_group, group_dist_mix_index])  
          + normal_lccdf(0 | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index], group_dist_sd[curr_assigned_dist_group, group_dist_mix_index]) +
          + log(group_dist_mix[curr_assigned_dist_group, group_dist_mix_index]); 
      }
    }
    
    target += log_sum_exp(group_dist_mix_lp);
  }

  if (sbc) {
    vector[num_dist_group_mix] group_dist_mix_lp;
    
    for (group_dist_mix_index in 1:num_dist_group_mix) {
      if (lognormal_dist_model) {
        group_dist_mix_lp[group_dist_mix_index] =
          lognormal_lpdf(sim_cluster_standard_dist[cluster_index] | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index] 
                                                                + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]] 
                                                                + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
                                                                group_dist_sd[curr_assigned_dist_group, group_dist_mix_index])
          + log(group_dist_mix[curr_assigned_dist_group, group_dist_mix_index]); 
      } else { // think this is wrong? TODO ed fix
        group_dist_mix_lp[group_dist_mix_index] = 
          normal_lpdf(sim_cluster_standard_dist[cluster_index] | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index] 
                                                             + county_dist_effect[curr_assigned_dist_group, cluster_county_id[cluster_index]] 
                                                             + cluster_dist_effect[curr_assigned_dist_group, cluster_index],
                                                             group_dist_sd[curr_assigned_dist_group, group_dist_mix_index])  
          + normal_lccdf(0 | group_dist_mean[curr_assigned_dist_group, group_dist_mix_index], group_dist_sd[curr_assigned_dist_group, group_dist_mix_index]) +
          + log(group_dist_mix[curr_assigned_dist_group, group_dist_mix_index]); 
      }
    }
    
    target += log_sum_exp(group_dist_mix_lp);
  }
}
