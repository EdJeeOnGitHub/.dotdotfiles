functions {
#include util.stan
}
data {
#include wtp_data.stan
    int<lower=0,upper=1> sbc;
}

transformed data {
#include wtp_transformed_data.stan
#include sbc/wtp_sbc.stan
}

parameters {
#include wtp_parameters.stan
}

transformed parameters {
#include wtp_transformed_parameters.stan
}

model {
#include wtp_model_section.stan
}

generated quantities {
    array[num_strata] int ranks_;
#include wtp_generated_quantities.stan

    for (k in 1:num_strata) {
        if (sbc) {
                ranks_[k] = strata_wtp_mu[k] > strata_wtp_mu_[k];
        } else {
                ranks_[k] = -1;
        }

    }
    

}
