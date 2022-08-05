
// dist_parameters_sec.stan
array[num_discrete_dist] simplex[num_dist_group_mix] group_dist_mix_;

array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_raw_;
array[num_discrete_dist] vector<lower = 0>[num_dist_group_mix] group_dist_sd_;

matrix[use_dist_county_effects ? num_discrete_dist : 0, num_counties] county_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] county_dist_effect_sd_;

matrix[use_dist_cluster_effects ? num_discrete_dist : 0, num_clusters] cluster_dist_effect_raw_;
vector<lower = 0>[use_dist_county_effects ? num_discrete_dist : 0] cluster_dist_effect_sd_;