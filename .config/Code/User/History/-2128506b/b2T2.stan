
// wtp_parameters.stan
real hyper_wtp_mu_;

vector[use_strata_levels ? num_strata : 0] raw_strata_wtp_mu_;
real<lower = 0> strata_wtp_mu_tau_;

real<lower = 0> wtp_sigma_;
array[num_wtp_obs] int<lower=-1,upper=1> sim_wtp_response; // -1 => KEEP

// simulate wtp_parameters.stan
hyper_wtp_mu_ = normal_rng(0, 2);

if (use_strata_levels) {
  for (k in 1:num_strata) {
    raw_strata_wtp_mu_[k] = std_normal_rng();
  }
}

strata_wtp_mu_tau_ = std_normal_rng();

wtp_sigma = std_normal_rng(); 
if (fit_wtp_model_to_data) { 
    // gift_choice = -1 => BRACELET, gift_choice = +1 => CALENDAR
    int wtp_stratum_pos = 1;
    vector[3] wtp_val_ = normal_rng(0, 1); // [BRACELET, NEITHER, CALENDAR]
    array[num_wtp_obs] int<lower=-1,upper=1> sim_gift_choice; 
    array[num_wtp_obs] vector[3] sim_gift_value; 
    array[num_strata] vector[3] strata_gift_value;

    for (stratum_index in 1:num_strata) {
      int curr_wtp_stratum_size = wtp_strata_sizes[stratum_index];
      int wtp_stratum_end = wtp_stratum_pos + curr_wtp_stratum_size - 1;
      strata_gift_value[stratum_index] = normal_rng(strata_wtp_mu_, strata_wtp_mu_tau_);

      for (wtp_obs_index in wtp_stratum_pos:wtp_stratum_end) {
        sim_gift_value[wtp_obs_index] = gumbel_rng(wtp_val_ + strata_gift_value[stratum_index], 1);
        // -2 so 1:3 => [-1, 0, 1]
        sim_gift_choice[wtp_obs_index] = which_max(sim_gift_value[wtp_obs_index]) - 2;
        sim_wtp_response[wtp_obs_index] = (-1)^((sim_gift_value[(sim_gift_choice[wtp_obs_index] + 2)] - sim_gift_value[(-1*sim_gift_choice[wtp_obs_index] + 2)] < wtp_offer) - 1);
      }
      wtp_stratum_pos = wtp_stratum_end + 1;
    }
}

// wtp_transformed_parameters.stan
vector[num_strata] strata_effect_wtp_mu_ = use_strata_levels ? raw_strata_wtp_mu_ * strata_wtp_mu_tau_ : rep_vector(0, num_strata);
vector[num_strata] strata_wtp_mu_ = hyper_wtp_mu_ + strata_effect_wtp_mu_;