vector<lower = 0, upper = 1>[num_preference_value_diff] prob_prefer_calendar;
matrix<lower = 0, upper = 1>[num_preference_value_diff, num_strata] strata_prob_prefer_calendar;

{
  int stratum_pos = 1;
  
  for (stratum_index in 1:num_strata) {
    int curr_stratum_size = wtp_strata_sizes[stratum_index];
    int stratum_end = stratum_pos + curr_stratum_size - 1;
    
    for (val_diff_index in 1:num_preference_value_diff) {
      strata_prob_prefer_calendar[val_diff_index, stratum_index] = 1 - normal_cdf(scaled_preference_value_diff[val_diff_index] | strata_wtp_mu[stratum_index], wtp_sigma);
    }
    
    stratum_pos = stratum_end + 1;
  }
}

prob_prefer_calendar =  strata_prob_prefer_calendar * strata_prop;