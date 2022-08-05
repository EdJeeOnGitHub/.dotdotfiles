
// wtp_parameters.stan
real hyper_wtp_mu_;

vector[use_strata_levels ? num_strata : 0] raw_strata_wtp_mu_;
real<lower = 0> strata_wtp_mu_tau_;

real<lower = 0> wtp_sigma_;
array[num_wtp_obs] int<lower=-1,upper=1> sim_wtp_response; // -1 => KEEP
// gift_choice = -1 => BRACELET, gift_choice = +1 => CALENDAR
array[num_wtp_obs] int sim_gift_choice; 


// simulate wtp_parameters.stan
hyper_wtp_mu_ = normal_rng(0, 2);

if (use_strata_levels) {
  for (k in 1:num_strata) {
    raw_strata_wtp_mu_[k] = std_normal_rng();
  }
}

strata_wtp_mu_tau_ = normal_lb_rng(0, 1, 0);

// wtp_transformed_parameters.stan
vector[num_strata] strata_effect_wtp_mu_ = use_strata_levels ? raw_strata_wtp_mu_ * strata_wtp_mu_tau_ : rep_vector(0, num_strata);
vector[num_strata] strata_wtp_mu_ = hyper_wtp_mu_ + strata_effect_wtp_mu_;

wtp_sigma_ = normal_lb_rng(0, 1, 0); 






if (fit_wtp_model_to_data) { 
    int wtp_stratum_pos = 1;
    array[num_wtp_obs] real valuation_difference; 

    sim_wtp_response = rep_array(-1, num_wtp_obs); 
    for (stratum_index in 1:num_strata) {
      int curr_wtp_stratum_size = wtp_strata_sizes[stratum_index];
      int wtp_stratum_end = wtp_stratum_pos + curr_wtp_stratum_size - 1;
      

      for (wtp_obs_index in wtp_stratum_pos:wtp_stratum_end) {
        valuation_difference[wtp_obs_index] = normal_rng(strata_wtp_mu_[stratum_index], wtp_sigma_);

        if (valuation_difference[wtp_obs_index] < 0) {
            sim_gift_choice[wtp_obs_index] = -1;
            if (-valuation_difference[wtp_obs_index] < scaled_wtp_offer[wtp_obs_index]) {
              sim_wtp_response[wtp_obs_index] = 1;
            }
        }  else {
            sim_gift_choice[wtp_obs_index] = 1;
            if (valuation_difference[wtp_obs_index] < scaled_wtp_offer[wtp_obs_index]) {
              sim_wtp_response[wtp_obs_index] = 1;
            }
        }
        
      }
      wtp_stratum_pos = wtp_stratum_end + 1;
    }
}
print(sim_wtp_response);
print(sim_gift_choice);
