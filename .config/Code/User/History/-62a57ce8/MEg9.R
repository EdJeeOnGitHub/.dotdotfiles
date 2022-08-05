#!/usr/bin/Rscript

# Setup -------------------------------------------------------------------

script_options <- docopt::docopt(sprintf(
"Usage:
  run_stan wtp [--analysis-data-only |[--num-chains=<num-chains> --num-iterations=<iterations> --adapt-delta=<adapt-delta> --max-treedepth=<max-treedepth>]] [--output-name=<output-name> --output-dir=<output-dir>]
  run_stan beliefs [--analysis-data-only |[--num-chains=<num-chains> --num-iterations=<iterations> --adapt-delta=<adapt-delta> --max-treedepth=<max-treedepth>]] [--output-name=<output-name> --output-dir=<output-dir>]
  run_stan dynamic [--analysis-data-only |[--gumbel --num-chains=<num-chains> --num-iterations=<iterations> --adapt-delta=<adapt-delta> --max-treedepth=<max-treedepth> --include-latent-var-data --save-sp-estimates]] [--separate-private-value --sms-control-only --include-name-matched --no-private-value-interact --model-levels=<model-levels> --use-cluster-re --output-name=<output-name> --output-dir=<output-dir>]
  run_stan static [--analysis-data-only |[--num-chains=<num-chains> --num-iterations=<iterations> --adapt-delta=<adapt-delta> --max-treedepth=<max-treedepth>]] [--separate-private-value --sms-control-only --include-name-matched --no-private-value-interact --model-levels=<model-levels> --use-cluster-identity-corr --use-cluster-re --use-census-covar --output-name=<output-name> --output-dir=<output-dir>]

 Options:
  --num-chains=<num-chains>, -c <num-chains>  Number of Stan chains [default: 4]
  --num-iterations=<iterations>, -i <iterations>  Number of sampling iterations [default: 300]
  --adapt-delta=<adapt-delta>, -d <adapt-delta>  Stan control adapt_delta [default: 0.8]
  --max-treedepth=<max-treedepth>, -t <max-treedepth>  Stan control max_treedepth [default: 15]
  --output-name=<output-name>, -o <output-name>  Name to use in stanfit .csv files and analysis data .RData file [default: param]
  --output-dir=<output-dir>, -p <output-dir>  Directory analysis data and stanfit output will be stored [default: %s/data]
  --analysis-data-only  Don't run sampling, just produce analysis data
  --sms-control-only  Exclude SMS treatment data
  --separate-private-value  Use separate private values for calendars and bracelets
  --include-name-matched  Include unmonitored sample (name matched against census)
  --no-private-value-interact  Allow private value to interact with distance
  --model-levels=<model-levels>  How deep is the multilevel model [default: 3]
  --use-cluster-re  Use cluster level random effects (if model levels <= 2)
  --use-cluster-identity-corr  No correlation estimation for cluster level parameters
  --use-census-covar  Control for census covariates
  --save-sp-estimates  Calcuate superpopulation ATE 
  --no-replicate  Do not generate outcome replication
  --gumbel, -g  Gumbel link
  --include-latent-var-data, -l  Save latent variable while sampling", getwd()),

  args = if (interactive()) "wtp -c 4 -i 2000" else commandArgs(trailingOnly = TRUE) 
)

library(magrittr)
library(plyr)
library(forcats)
library(broom)
library(tidyverse)
library(modelr)
library(rstan)

source("analysis_util.R")

fit_version <- script_options$output_name 

# Load Data ---------------------------------------------------------------

load(file.path("data", "analysis.RData"))

# Setup for Analysis Data -----------------------------------------------------------

subgroups <- "phone_owner"

stan_analysis_data <- analysis.data %>%
  mutate(private_value = fct_collapse(assigned.treatment, 
                                      control = if (!is_null(script_options) && script_options$separate_private_value) c("control", "ink", "bracelet") else c("control", "ink"),
                                      calendar = if (!is_null(script_options) && script_options$separate_private_value) "calendar" else c("calendar", "bracelet")),
         social_value = fct_collapse(assigned.treatment, control = c("control", "calendar")),
         sms.treatment.2 = fct_recode(sms.treatment.2, control = "sms.control")) %>% 
  add_count(cluster.id, name = "cluster_pop_size") %>% 
  add_count(cluster.id, village, name = "village_pop_size") 

all_ate <- get_dyn_ate() 
  # bind_rows(Busia = ., Kakamega = ., Siaya = ., .id = "stratum")

if (script_options$no_private_value_interact %||% FALSE) {
  treatment_formula <- ~ (private_value + social_value * dist.pot.group) * phone_owner
} else {
  treatment_formula <- ~ (private_value + social_value) * dist.pot.group * phone_owner
}

if (script_options$separate_private_value %||% FALSE) {
  all_ate %<>% 
    mutate(
      private_value_left = if_else(social_value_left == "bracelet", "control", private_value_left),
      private_value_right = if_else(social_value_right == "bracelet", "control", private_value_right),
      incentive_shift_left = if_else(signal_observed_left == "bracelet", "control", incentive_shift_left),
      incentive_shift_right = if_else(signal_observed_right == "bracelet", "control", incentive_shift_right))
}

treatment_map_var <- c("private_value", "social_value", "dist.pot.group")
base_treatment_map_filter <- . %>% 
  filter(private_value == "control" | social_value != "ink") 
treatment_map_filter <- base_treatment_map_filter

exclude_pars <- NA

if (script_options$beliefs) {
  exclude_pars <- c("obs_recognized_intercept_raw", "obs_1ord_beta_raw", "obs_2ord_beta_raw", "obs_recognized_intercept", "obs_1ord_beta", "obs_2ord_beta", "recognized_latent_var",
                    "rep_know_table_A_prop_recognized", "degree", "beliefs_1ord_latent_var", "beliefs_2ord_latent_var", 
                    "rep_know_table_A_1ord_prop_know", "beliefs_1ord_prop_know",
                    "rep_know_table_A_2ord_prop_know", "beliefs_2ord_prop_know")
}

if (script_options$dynamic %||% FALSE) {
  if (script_options$sms_control_only %||% FALSE) {
    stan_analysis_data %<>% 
      filter(!name_matched | script_options$include_name_matched, 
             sms.treatment.2 == "control") 
    
    if (!is_null(script_options) && script_options$include_name_matched) {
      subgroups %<>% c("name_matched")
      
      # static_treatment_map <- stan_analysis_data %>% 
      #   data_grid(private_value, social_value, dist.pot.group, phone_owner, name_matched) %>% 
      #   filter(private_value == "control" | social_value != "ink") %>%
      #   prepare_treatment_map()
      
      treatment_formula %<>% 
        update.formula(~ . * name_matched)
    } else {
      # static_treatment_map <- stan_analysis_data %>% 
      #   data_grid(private_value, social_value, dist.pot.group, phone_owner) %>% 
      #   filter(private_value == "control" | social_value != "ink") %>%
      #   prepare_treatment_map()
      
       all_ate %<>% 
        filter(!name_matched) %>% 
        select(-name_matched) %>% 
        filter(sms.treatment.2_left == "control", sms.treatment.2_right == "control") %>% 
        select(-starts_with("reminder_info_stock"), -starts_with("sms.treatment")) %>% 
        distinct()
    }
  } else {
    stan_analysis_data %<>% 
      filter(!name_matched | ((script_options$include_name_matched %||% TRUE) & sms.treatment.2 == "control")) 
    
    if (script_options$include_name_matched %||% TRUE) {
      subgroups %<>% c("name_matched")
      
      treatment_map_filter <- . %>% 
        filter(sms.treatment.2 == "control" | phone_owner,
               !name_matched | sms.treatment.2 == "control",
               sms.treatment.2 != "reminder.only" | (private_value == "control" & social_value == "control")) %>% 
        base_treatment_map_filter() 
      
      treatment_formula %<>% 
        update.formula(~ . * name_matched)
    } else {
      treatment_map_filter <- . %>% 
        filter(sms.treatment.2 == "control" | phone_owner,
               sms.treatment.2 != "reminder.only" | (private_value == "control" & social_value == "control")) %>% 
        base_treatment_map_filter() 
      
       all_ate %<>% 
        filter(!name_matched) %>% 
        select(-name_matched) 
    }
    
    treatment_map_var %<>% c("sms.treatment.2")
    
    treatment_formula %<>% 
      update.formula(~ . + (social_value * dist.pot.group) : sms.treatment.2 + sms.treatment.2)
  }
} else { # STATIC
  all_ate %<>% 
    select(-starts_with("reminder_info_stock"), -starts_with("signal_observed"), -starts_with("incentive_shift"), -starts_with("dyn_dist_pot")) 
  
  if (script_options$sms_control_only %||% FALSE) {
    stan_analysis_data %<>% 
      filter(!name_matched | script_options$include_name_matched, 
             sms.treatment.2 == "control") 
  
    if (script_options$include_name_matched) {
      subgroups %<>% c("name_matched")
      
      treatment_formula %<>% 
        update.formula(~ . * name_matched)
    } else {
       all_ate %<>% 
        filter(!name_matched) %>% 
        select(-name_matched) 
    }
    
    all_ate %<>%
      filter(sms.treatment.2_left == "control", sms.treatment.2_right == "control") %>% 
      select(-starts_with("sms.treatment")) 
  } else {
    stan_analysis_data %<>% 
      filter(!name_matched | ((script_options$include_name_matched %||% TRUE) & sms.treatment.2 == "control")) 
  
    if (script_options$include_name_matched %||% TRUE) {
      subgroups %<>% c("name_matched")
      
      treatment_map_filter <- . %>% 
        filter(sms.treatment.2 == "control" | phone_owner,
               !name_matched | sms.treatment.2 == "control",
               sms.treatment.2 != "reminder.only" | (private_value == "control" & social_value == "control")) %>% 
        base_treatment_map_filter() 
      
      treatment_formula %<>% 
        update.formula(~ . * name_matched)
    } else {
      treatment_map_filter <- . %>% 
        filter(sms.treatment.2 == "control" | phone_owner,
               sms.treatment.2 != "reminder.only" | (private_value == "control" & social_value == "control")) %>% 
        base_treatment_map_filter() 
      
       all_ate %<>% 
        filter(!name_matched) %>% 
        select(-name_matched) 
    }
    
    if (!script_options$beliefs) {
      treatment_map_var %<>% c("sms.treatment.2")
      
      treatment_formula %<>% 
        update.formula(~ . + (social_value * dist.pot.group) : sms.treatment.2 + sms.treatment.2)
    } else {
      all_ate %<>%
        filter(sms.treatment.2_left == "control", sms.treatment.2_right == "control") %>% 
        select(-starts_with("sms.treatment")) 
      
      treatment_map_filter <- base_treatment_map_filter 
    }
  }
  
  all_ate %<>% 
    distinct()
}

# if (script_options$`use-census-covar` %||% TRUE) {
#   treatment_formula %<>% 
#     update.formula(~ . + age.census_group * gender)
#   
#   subgroups %<>% c("age.census_group", "gender")
#   
#   all_ate %<>% 
#     merge(distinct(stan_analysis_data, age.census_group, gender), all = TRUE)
# }

treatment_map_var %<>% c(subgroups)

cat(str_interp("Design matrix variables: ${str_c(treatment_map_var, collapse = ', ')}\n"))

static_treatment_map <- stan_analysis_data %>% 
  expand(!!!syms(treatment_map_var)) %>%
  treatment_map_filter() %>% 
  prepare_treatment_map()

# Generate Analysis Data --------------------------------------------------

param_stan_data <- prepare_bayesian_analysis_data(
  stan_analysis_data,
  wtp.data, 
  endline.know.table.data,
   
  prepared_treatment_maps = TRUE, 
  treatment_map = static_treatment_map,
    
  treatment_formula = treatment_formula,
  subgroup_col = subgroups,
  drop_intercept_from_dm = FALSE, 
  param_dynamics = script_options$dynamic %||% FALSE,
  param_poly_order = 2,
  
  all_ate = all_ate,
 
  scale_sigma = 1,
  cluster_scale_sigma = 0.25,
  hyper_coef_sigma = 1,
  hyper_intercept_sigma = 5,
  private_value_dist_sigma = 0.01,
  
  lkj_df = 2,
  
  # dynamic_model = script_options$dynamic %||% FALSE,
  
  model_link_type = !((script_options$dynamic %||% FALSE) && script_options$gumbel),
  
  estimate_ate = 1,
  replicate_outcomes = !(script_options$no_replicate %||% FALSE),
  
  model_levels = as.integer(script_options$model_levels %||% 3),
  use_cluster_re = as.integer(script_options$use_cluster_re %||% 0),
  use_cluster_identity_corr = as.integer(script_options$use_cluster_identity_corr %||% 0),
  save_sp_estimates = as.integer(script_options$save_sp_estimates %||% 0),
  
  use_census_covar = as.integer(script_options$use_census_covar %||% TRUE)
)

save(param_stan_data, script_options, 
     file = file.path(script_options$output_dir, "stan_analysis_data", str_interp("model_${fit_version}.RData")))

if (script_options$analysis_data_only) quit()

# Initializer Factory -------------------------------------------------------------

gen_initializer <- function(stan_data_list, script_options = script_options) {
  if (script_options$beliefs) {
    function() {
      lst(recognized_prop_alpha = 0.1,
          recognized_prop_beta = 0.1)
    }
    
  } else if (script_options$dynamic) { # DYNAMIC
    function() {
      init_lst <- lst(
        # strata_beta_day1_corr_mat_non_phone = with(stan_data_list, rethinking::rlkjcorr(1, subgroup_treatment_col_sizes[1], lkj_df)),
        # strata_beta_day1_corr_mat_phone = with(stan_data_list, rethinking::rlkjcorr(1, subgroup_treatment_col_sizes[2], lkj_df)),
        # strata_beta_day1_L_corr_mat_non_phone = t(chol(strata_beta_day1_corr_mat_non_phone)),
        # strata_beta_day1_L_corr_mat_phone = t(chol(strata_beta_day1_corr_mat_phone)),
        
        # hyper_beta_day1 = rep(0, stan_data_list$num_all_treatment_coef),
        # hyper_beta_day1 = rnorm(stan_data_list$num_all_treatment_coef),
        # hyper_baseline_dyn_effect = rep(0, stan_data_list$num_deworming_days - 1),
        # hyper_baseline_dyn_effect = rnorm(stan_data_list$num_deworming_days - 1),
        # hyper_treat_beta_dyn_effect = rep(0, stan_data_list$num_param_dyn_coef),
        strata_beta_tau = with(stan_data_list, pmax(0.001, rnorm(num_all_treatment_coef, sd = 0.5))), 
        strata_baseline_dyn_effect_raw = with(stan_data_list, matrix(rnorm((num_deworming_days - 1) * num_strata, sd = 0.5), nrow = num_deworming_days - 1, ncol = num_strata)),
        # QR_strata_beta_day1 = with(stan_data_list, matrix(rnorm(num_all_treatment_coef * num_strata, sd = 0.005), nrow = num_all_treatment_coef, ncol = num_strata)),
        QR_strata_beta_dyn_effect = with(stan_data_list, matrix(rnorm(num_param_dyn_coef * num_strata, sd = 0.005), nrow = num_param_dyn_coef, ncol = num_strata)),
        strata_beta_corr_mat = with(stan_data_list, rethinking::rlkjcorr(1, num_all_treatment_coef, lkj_df)),
        strata_beta_L_corr_mat = t(chol(strata_beta_corr_mat))
      )
      
      if (script_options$model_levels > 2) {
        init_lst %<>% 
          update_list(cluster_beta = with(stan_data_list, matrix(rnorm(num_all_treatment_coef * num_clusters, sd = 0.01), 
                                                                 nrow = num_all_treatment_coef, ncol = num_clusters)))
      }
    }
  } else { # STATIC
    if (script_options$model_levels > 2) {
      function() {
        lst(cluster_beta = with(stan_data_list, matrix(rnorm(num_all_treatment_coef * num_clusters, sd = 0.01), 
                                                       nrow = num_all_treatment_coef, ncol = num_clusters)),
            strata_beta_corr_mat = with(stan_data_list, diag(rep(1, num_all_treatment_coef))),
            strata_beta_L_corr_mat = t(chol(strata_beta_corr_mat)),
            strata_beta_tau = with(stan_data_list, pmax(0.001, rnorm(num_all_treatment_coef, sd = 0.5)))) 
      }
    } else return("random")
  }
}

# Run ---------------------------------------------------------------------

num_chains <- as.integer(script_options$num_chains)

if (num_chains > parallel::detectCores()) stop("Not enough cores.")

options(mc.cores = num_chains)
rstan_options(auto_write = TRUE)

cat(str_interp("Output name: ${fit_version}\n"))

if (script_options$wtp) {
  model_param <- stan_model(file = file.path("stan_models", "wtp_model.stan"), model_name = "wtp_model")
} else if (script_options$beliefs) {
  model_param <- stan_model(file = file.path("stan_models", "secobeliefs.stan"), model_name = "beliefs")
} else {
  model_param <- stan_model(file = file.path("stan_models", "takeup_model_5_param.stan"), model_name = "model_5_param")
  
  cat(str_interp("Running model with ${param_stan_data$model_levels} levels.\n"))
}

model_fit <- param_stan_data %>% 
  sampling(model_param, 
           data = ., 
           chains = num_chains,
           iter = as.integer(script_options$num_iterations),
           control = lst(max_treedepth = as.integer(script_options$max_treedepth), 
                         adapt_delta = as.numeric(script_options$adapt_delta)), 
           init = gen_initializer(., script_options),
           include = FALSE,
           pars = exclude_pars,
           sample_file = file.path(script_options$output_dir, "stanfit", str_interp("model_${fit_version}.csv")))

write_rds(model_fit, file.path(script_options$output_dir, "stanfit", str_interp("model_${fit_version}.rds")))

cat(str_interp("Sampling complete for ${fit_version}.\n"))
