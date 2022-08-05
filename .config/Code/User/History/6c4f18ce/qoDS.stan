functions {
#include util.stan
#include beliefs_functions.stan
#include takeup_functions.stan
}

data { 
#include base_data_sec.stan
#include beliefs_data_sec.stan
#include dist_data_sec.stan
    int<lower=0,upper=1> sbc;
}

transformed data {
#include base_transformed_data.stan
#include beliefs_transformed_data.stan
#include sbc/dist_sbc.stan
#include sbc/beliefs_sbc.stan
}

parameters {
#include dist_parameters_sec.stan
#include beliefs_parameters_sec.stan
}

transformed parameters {
#include dist_transformed_parameters.stan
#include beliefs_transformed_parameters.stan
}

model {
#include dist_model_sec.stan
#include beliefs_model_sec.stan
}

generated quantities {
#include dist_generated_quantities.stan
#include beliefs_generated_quantities.stan
    array[num_beliefs_obs, num_treatments] int centered_obs_beta_1ord_ranks_;
    array[num_discrete_dist] ordered[num_dist_group_mix] group_dist_mean_ranks_;

    for (k in 1:num_beliefs_obs) {
        for (j in 1:num_treatments) {
            if (sbc) {
                centered_obs_beta_1ord_ranks_[k, j] = centered_obs_beta_1ord_[k, j] < centered_obs_beta_1ord[k, j];
            } else {
                centered_obs_beta_1ord_ranks_[k, j] = -1;
            }
        }
    }

    for (k in 1:num_discrete_dist) {
        for (j in 1:num_dist_group_mix) {
            if (sbc) {
                group_dist_mean_ranks_[k, j] = group_dist_mean_[k, j] < group_dist_mean[k, j];
            } else {
                group_dist_mean_ranks[k, j] = -1;
            }
        }
    }

}

