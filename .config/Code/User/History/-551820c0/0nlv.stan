// SBC
array[num_obs] int<lower = 0, upper = 1> sim_takeup; // Observed outcome variable
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
profile("Sim priors") {

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
        reduced_beta_age_group_alpha_[k]= normal_lb_rng(0.0, age_group_alpha_sd[k], 0.0);
    }

    if (use_age_group_gp) {
        for (k in 1:num_dist_group_treatments){
            reduced_beta_age_group_rho_[k] = normal_lb_rng(0.0, age_group_rho_sd[k], 0.0);
        }
    }
}
}

// transforming_ parameters
vector[num_dist_group_treatments] beta_; 
vector[num_dist_group_treatments] reduced_treatment_effect_;
// matrix[num_clusters, num_age_groups] reduced_cluster_benefit_cost;
vector[num_clusters] reduced_cluster_benefit_cost_;
vector[num_clusters] reduced_beta_cluster_ = rep_vector(0, num_clusters);
matrix[num_counties, num_dist_group_treatments] reduced_beta_county_ = rep_matrix(0, num_counties, num_dist_group_treatments);
matrix[num_age_groups, num_dist_group_treatments] reduced_beta_age_group_ = rep_matrix(0, num_age_groups, num_dist_group_treatments);

matrix<lower = 0, upper = 1>[num_clusters, num_age_groups] reduced_cluster_takeup_prob_;
profile("sim transformed params") {

for (dist_index in 1:num_discrete_dist) {
beta_[(num_treatments * (dist_index - 1) + 1):(num_treatments * dist_index)] = 
    [ beta_control_[dist_index], beta_ink_effect_[dist_index], beta_calendar_effect_[dist_index], beta_bracelet_effect_[dist_index] ]';
}

reduced_treatment_effect_ = treatment_map_design_matrix * beta_;
// reduced_cluster_benefit_cost = rep_matrix(reduced_treatment_effect[cluster_assigned_dist_group_treatment], num_age_groups);
reduced_cluster_benefit_cost_ = reduced_treatment_effect_[cluster_assigned_dist_group_treatment];

if (use_cluster_effects) {
reduced_beta_cluster_ = reduced_beta_cluster_raw_ * reduced_beta_cluster_sd_;

// reduced_cluster_benefit_cost += rep_matrix(reduced_beta_cluster, num_age_groups);
reduced_cluster_benefit_cost_ += reduced_beta_cluster_;
}

if (use_county_effects) {
vector[num_clusters] county_effects_;

reduced_beta_county_ = reduced_beta_county_raw_ .* rep_matrix(reduced_beta_county_sd_, num_counties);

county_effects_ = rows_dot_product(cluster_treatment_design_matrix, reduced_beta_county_[cluster_county_id]); 
// reduced_cluster_benefit_cost += rep_matrix(county_effects, num_age_groups);
reduced_cluster_benefit_cost_ += county_effects_;
}

if (use_age_groups) {
if (use_age_group_gp) { 
    for (treatment_index in 1:num_dist_group_treatments) {
    reduced_beta_age_group_[, treatment_index] = calc_gp_trend(age_groups_dist, reduced_beta_age_group_alpha_[treatment_index], reduced_beta_age_group_rho_[treatment_index], reduced_beta_age_group_raw_[, treatment_index]);
    }
} else {
    reduced_beta_age_group_ = reduced_beta_age_group_raw_ .* rep_matrix(reduced_beta_age_group_alpha_, num_age_groups);
}
}

for (age_group_index in 1:num_age_groups) {
reduced_cluster_takeup_prob_[, age_group_index] = Phi_approx(reduced_cluster_benefit_cost_ + cluster_treatment_design_matrix * reduced_beta_age_group_[age_group_index]');
}
}


profile("sim takeup") {

sim_takeup = bernoulli_rng(
Phi_approx(
    reduced_cluster_benefit_cost_[obs_cluster_id] + rows_dot_product(cluster_treatment_design_matrix[obs_cluster_id], reduced_beta_age_group_[obs_age_group])
)
);
}