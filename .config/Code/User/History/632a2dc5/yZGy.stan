data {
#include wtp_data.stan
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
#include wtp_generated_quantities.stan
}
