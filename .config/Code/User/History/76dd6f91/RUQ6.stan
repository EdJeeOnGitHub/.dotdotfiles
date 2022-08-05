/*
################################################################################
Parameters
################################################################################
*/

// Levels: control ink calendar bracelet
real beta_intercept_;
real beta_ink_effect_;
real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_calendar_effect_;
real<lower = (use_private_incentive_restrictions ? 0 : negative_infinity())> beta_bracelet_effect_;

matrix[use_cluster_effects ? num_clusters : 0, num_treatments] structural_beta_cluster_raw_;
row_vector<lower = 0>[use_cluster_effects ? num_treatments : 0] structural_beta_cluster_sd_;

matrix[use_county_effects ? num_counties : 0, num_treatments] structural_beta_county_raw_;
row_vector<lower = 0>[use_county_effects ? num_treatments : 0] structural_beta_county_sd_;

// Reputational Returns

real<lower = 0> base_mu_rep_;
vector<lower = 0>[use_homoskedastic_shocks ? 1 : num_treatment_shocks] raw_u_sd_;

// Linear Parametric Cost

vector[num_dist_param] dist_beta_v_; // Linear distance*treatment effects

matrix[use_param_dist_cluster_effects ? num_clusters : 0, num_dist_param] dist_beta_cluster_raw_;
row_vector<lower = 0>[use_param_dist_cluster_effects ? num_dist_param : 0] dist_beta_cluster_sd_;

matrix[use_param_dist_county_effects ? num_counties : 0, num_dist_param] dist_beta_county_raw_;
row_vector<lower = 0>[use_param_dist_county_effects ? num_dist_param : 0] dist_beta_county_sd_;

// Quadratic Cost Model

vector<lower = 0>[num_dist_param_quadratic] dist_quadratic_beta_v_; // Quadratic distance*treatment effects

// WTP valuation parameters
real<lower = 0> wtp_value_utility_;

/*
################################################################################
Transformed Parameters
################################################################################
*/
// Transfor