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

// drawing from priors
beta_control_ = to_vector(normal_rng(0, [ beta_control_sd, beta_far_effect_sd ]'));
beta_ink_effect_ = to_vector(normal_rng(0, [ beta_ink_effect_sd, beta_far_ink_effect_sd ]'));
beta_calendar_effect_ = to_vector(normal_rng(0, [ beta_calendar_effect_sd, beta_far_calendar_effect_sd ]'));
beta_bracelet_effect_ = to_vector(normal_rng(0, [ beta_bracelet_effect_sd, beta_far_bracelet_effect_sd ]'));

if (use_cluster_effects) {
reduced_beta_cluster_raw_ = to_vector(normal_rng(rep_vector(0.0, num_clusters), rep_vector(1.0, num_clusters)));
}

reduced_beta_cluster_sd_ = normal_lb_rng(0, reduced_beta_cluster_sd_sd, 0);

if (use_county_effects) {
for (k in 1:num_dist_group_treatments) {
    reduced_beta_county_raw_[, k] = multi_normal_rng(rep_vector(0, num_counties), identity_matrix(num_counties));
    reduced_beta_county_sd_[k] = normal_lb_rng(0, reduced_beta_county_sd_sd, 0);
}
}

if (use_age_groups) {
for (k in 1:num_dist_group_treatments) {
    reduced_beta_age_group_raw_[, k] = multi_normal_rng(rep_vector(0, num_age_groups), identity_matrix(num_age_groups));
}
    reduced_beta_age_group_alpha_ = to_row_vector(normal_rng(0, age_group_alpha_sd));

if (use_age_group_gp) {
    for (k in 1:num_dist_group_treatments){
    reduced_beta_age_group_rho_[k] = normal_lb_rng(0.0, age_group_rho_sd[k], 0.0);
    }
}
}