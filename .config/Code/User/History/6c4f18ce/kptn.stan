functions {
#include util.stan
#include beliefs_functions.stan
#include takeup_functions.stan
}

data { 
#include base_data_sec.stan
#include beliefs_data_sec.stan
#include dist_data_sec.stan
}

transformed data {
#include base_transformed_data.stan
#include beliefs_transformed_data.stan
#include sbc/dist_sbc.stan
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
}

