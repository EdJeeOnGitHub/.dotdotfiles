hyper_wtp_mu ~ normal(0, 2);

if (use_strata_levels) {
  raw_strata_wtp_mu ~ std_normal();
}

strata_wtp_mu_tau ~ normal(0, 1);

wtp_sigma ~ normal(0, 1); 

if (fit_wtp_model_to_data & !sbc) { 
  int wtp_stratum_pos = 1;

  for (stratum_index in 1:num_strata) {
    int curr_wtp_stratum_size = wtp_strata_sizes[stratum_index];
    int wtp_stratum_end = wtp_stratum_pos + curr_wtp_stratum_size - 1;

    for (wtp_obs_index in wtp_stratum_pos:wtp_stratum_end) {
      if (wtp_response[wtp_obs_index] == -1) { // Decided to KEEP
        if (gift_choice[wtp_obs_index] == -1) { // Initial choice: BRACELET
          target += log(Phi_approx((- scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma));
        } else { // Initial choice: CALENDAR
          target += log(1 - Phi_approx((scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma));
        }
      } else { // Decided to SWITCH
        target += log(gift_choice[wtp_obs_index] *
          (Phi_approx((gift_choice[wtp_obs_index] * scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma) - Phi_approx(- strata_wtp_mu[stratum_index] / wtp_sigma)));
      }
    }

    wtp_stratum_pos = wtp_stratum_end + 1;
  }
}

if (sbc) {
  int wtp_stratum_pos = 1;

  for (stratum_index in 1:num_strata) {
    int curr_wtp_stratum_size = wtp_strata_sizes[stratum_index];
    int wtp_stratum_end = wtp_stratum_pos + curr_wtp_stratum_size - 1;

    for (wtp_obs_index in wtp_stratum_pos:wtp_stratum_end) {
      if (sim_wtp_response[wtp_obs_index] == -1) { // Decided to KEEP
        if (sim_gift_choice[wtp_obs_index] == -1) { // Initial choice: BRACELET
          target += log(Phi_approx((- scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma));
        } else { // Initial choice: CALENDAR
          target += log(1 - Phi_approx((scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma));
        }
      } else { // Decided to SWITCH
        target += log(sim_gift_choice[wtp_obs_index] *
          (Phi_approx((sim_gift_choice[wtp_obs_index] * scaled_wtp_offer[wtp_obs_index] - strata_wtp_mu[stratum_index]) / wtp_sigma) - Phi_approx(- strata_wtp_mu[stratum_index] / wtp_sigma)));
      }
    }

    wtp_stratum_pos = wtp_stratum_end + 1;
  }
}