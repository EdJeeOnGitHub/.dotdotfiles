
# Model Classes -----------------------------------------------------------

# setClass("TakeUpModel", 
#          contains = "stanmodel",
#          slots = c("pars" = "character"))
# 
# setClass("ReducedFormTakeUpModel",  
#          contains = "TakeUpModel")
# 
# setClass("StructuralTakeUpModel",  
#          contains = "TakeUpModel")
# 
# create_initializer <- function(model_file, pars) {
#   compiled_model <- stan_model(model_file)
#   
#   function(.Object, ...) {
#     .Object <- callNextMethod()
#     
#     .Object@pars <- pars 
#     return(.Object)
#   }
# }
# 
# setMethod("initialize", "ReducedFormTakeUpModel", 
#           create_initializer(file.path("stan_models", "takeup_reduced.stan"), 
#                              pars = c("structural_cluster_benefit_cost", "structural_cluster_obs_v", "structural_cluster_takeup_prob", "beta", "cluster_cf_benefit_cost", "cluster_rep_benefit_cost")))
# 
# setMethod("initialize", "StructuralTakeUpModel", 
#           create_initializer(file.path("stan_models", "dist_struct_fixedpoint.stan"), 
#                              pars = c("total_error_sd", "cluster_dist_cost", "structural_cluster_benefit_cost", "structural_cluster_obs_v", "structural_cluster_takeup_prob",
#                                       "beta", "dist_beta_v", "mu_rep", "cluster_cf_benefit_cost", "mu_cluster_effects_raw", "mu_cluster_effects_sd", "cluster_mu_rep", 
#                                       "cluster_rep_benefit_cost", "sim_benefit_cost",
#                                       "group_dist_mean", "group_dist_sd", "group_dist_mix",
#                                       "dist_beta_county_raw", "dist_beta_county_sd")))

# Functions ---------------------------------------------------------------

get_spline_range <- function(x) {
  lst(lower_range = 1.01 * min(x) - 0.01 * max(x),
      upper_range = 1.01 * max(x) - 0.01 * min(x))
}

calculate_splines <- function(x, num_interior_knots, splines_for = x, spline_type = c("osullivan", "i-spline", "b-spline"), ...) {
  spline_type <- match.arg(spline_type)
  
  spline_range <- get_spline_range(x)
  
  interior_knots <- quantile(unique(x), seq(0, 1, length = num_interior_knots + 2)) %>% 
    magrittr::extract(2:(num_interior_knots + 1))

  switch(
    spline_type,
    
    "osullivan" = ZOSull(splines_for, range.x = unlist(spline_range), intKnots = interior_knots, ...),
    "i-spline" = iSpline(splines_for, knots = interior_knots,  Boundary.knots = unlist(spline_range), ...),
    "b-spline" = bSpline(splines_for, knots = interior_knots,  Boundary.knots = unlist(spline_range), ...)
  )
}

extract_sim_level <- function(fit, par, stan_data, quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95)) {
  analysis_data <- stan_data$analysis_data
  dist_as_data <- is.data.frame(stan_data$grid_dist)
  
  fit %>% 
    as.data.frame(par = par) %>% 
    mutate(iter_id = seq_len(n())) %>% 
    gather(indices, iter_est, -iter_id) %>% 
    tidyr::extract(indices, c("grid_index", "assigned_treatment"), "(\\d+),(\\d+)", convert = TRUE) %>% 
    group_nest(grid_index, assigned_treatment, .key = "iter_data") %>% 
    mutate(mean_est = map_dbl(iter_data, ~ mean(.$iter_est)),
           quantiles_est = map(iter_data, quantilize_est, 
                               iter_est,
                               quant_probs = quant_probs),
           assigned_treatment = factor(assigned_treatment, labels = levels(analysis_data$assigned.treatment))) %>% 
    arrange(assigned_treatment, grid_index) %>%
    mutate(assigned_dist_standard = rep(stan_data$grid_dist, nlevels(assigned_treatment)),
           assigned_dist = unstandardize(assigned_dist_standard, analysis_data$cluster.dist.to.pot))
}

extract_rep_level <- function(fit, par, stan_data, quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95), thin = 1, dewormed_var = dewormed) {
  analysis_data <- stan_data$analysis_data
  
  fit_data <- fit %>% 
    as.array(par = par) %>% {
      if (thin > 1) magrittr::extract(., (seq_len(nrow(.)) %% thin) == 0,,) else .
    } %>% 
    plyr::adply(3, function(cell) tibble(iter_data = list(cell)) %>% mutate(ess_bulk = if (thin > 1) ess_bulk(cell), 
                                                                            ess_tail = if (thin > 1) ess_tail(cell),
                                                                            rhat = if (thin > 1) Rhat(cell),)) %>% 
    tidyr::extract(parameters, "cluster_id", "(\\d+)", convert = TRUE) %>%  
    mutate(iter_data = map(iter_data, ~ tibble(iter_est = c(.), iter_id = seq(nrow(.) * ncol(.)))),
           assigned_dist_standard = stan_data$cluster_standard_dist[cluster_id],
           assigned_dist = unstandardize(assigned_dist_standard, analysis_data$cluster.dist.to.pot)) %>% 
    # left_join(stan_data$analysis_data %>% count(cluster_id, assigned_treatment = assigned.treatment, name = "cluster_size"), by = "cluster_id") 
    left_join(stan_data$analysis_data %>%
                select(cluster_id, assigned_treatment = assigned.treatment, dewormed) %>%
                add_count(cluster_id, name = "cluster_size") %>% 
                group_by(cluster_id, assigned_treatment, cluster_size) %>%
                summarize(obs_num_takeup = sum({{ dewormed_var }})) %>%
                ungroup(),
              by = c("cluster_id")) 
  
  fit_data %<>% 
    as_tibble()
}

extract_obs_cluster_age_group_cutoff_cf <- function(fit, stan_data, model_type = "reduced form", quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95), dewormed_var = dewormed, always_diagnose = TRUE) {
  if (model_type != "reduced form") return(NULL)
  
  analysis_data <- stan_data$analysis_data
  
  cluster_treatment_map <- stan_data$cluster_treatment_map %>% 
    mutate(treatment_index = seq(n()))
  
  is_stanfit <- is(fit, "stanfit")
  
  fit %>% {
    if (is_stanfit) {
      stop("Only supports cmdstanr")
    } else if (is_tibble(.)) {
      filter(., str_detect(variable, r"{^cluster_age_group_cf_cutoff\[.+\]$}"))
    } else {
      .$draws("cluster_age_group_cf_cutoff") %>% 
        posterior::as_draws_df() %>% 
        recover_types(rename(analysis_data, age_group = census.age_group)) %>% 
        gather_draws(cluster_age_group_cf_cutoff[treatment_index, mu_treatment_index, cluster_id, age_group]) %>% 
        mutate(iter_id = .draw) %>% 
        rename(iter_est = .value) %>% 
        # pivot_longer(!c(iter_id, .draw, .iteration, .chain), names_to = "variable", values_to = "iter_est") %>% 
        # nest(iter_data = !variable)
        nest(iter_data = c(iter_id, .draw, .iteration, .chain, iter_est))
    }
  } %>%   
    tidyr::extract(variable, c("treatment_index", "mu_treatment_index", "cluster_id", "age_group"), r"{(\d+),(\d+),(\d+)(?:,(\d+))?}", convert = TRUE) %>%  
    mutate(
      treatment_index_obs = stan_data$cluster_assigned_dist_group_treatment[cluster_id],
      assigned_dist_standard_obs = stan_data$cluster_standard_dist[cluster_id],
      assigned_dist_obs = unstandardize(assigned_dist_standard_obs, analysis_data$cluster.dist.to.pot),
      mu_assigned_treatment = factor(mu_treatment_index, levels = 1:4, labels = levels(stan_data$cluster_assigned_treatment)), 
      age_group = factor(age_group, labels = levels(analysis_data$age.census_group))
    ) %>% 
    left_join(cluster_treatment_map, by = "treatment_index") %>% 
    left_join(cluster_treatment_map, by = c("treatment_index_obs" = "treatment_index"), suffix = c("", "_obs")) %>% 
    left_join(analysis_data %>% count(cluster_id, age_group = age.census_group, name = "cluster_size"), by = c("cluster_id", "age_group")) %>% 
    left_join(
      stan_data$analysis_data %>%
        select(cluster_id, age_group = age.census_group, assigned_treatment = assigned.treatment, assigned_dist_group = dist.pot.group, {{ dewormed_var }}) %>%
        group_by(cluster_id, age_group, assigned_treatment, assigned_dist_group) %>%
        summarize(obs_num_takeup = sum({{ dewormed_var }}), .groups = "drop"),
      by = c("cluster_id", "age_group", "assigned_treatment", "assigned_dist_group")
    ) %>%
    mutate(
      # obs_num_takeup = if_else(is.na(mu_assigned_treatment) | treatment_index == mu_treatment_index, obs_num_takeup, NA_integer_)
      obs_num_takeup = if_else(fct_match(model_type, "reduced form") | is.na(mu_assigned_treatment) | assigned_treatment == mu_assigned_treatment, obs_num_takeup, NA_integer_),
      obs_prop_takeup = obs_num_takeup / cluster_size
    ) %>% 
    as_tibble()
}

extract_obs_cluster_cutoff_cf <- function(fit, stan_data, model_type = "structural", quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95), dewormed_var = dewormed, always_diagnose = TRUE) {
  analysis_data <- stan_data$analysis_data
  
  cluster_treatment_map <- stan_data$cluster_treatment_map %>% 
    mutate(treatment_index = seq(n()))
  
  is_stanfit <- is(fit, "stanfit")
  
  fit %>% {
    if (is_stanfit) {
      as.array(., par = "cluster_cf_cutoff") %>% 
        plyr::adply(3, function(cell) tibble(iter_data = list(cell)) %>% mutate(ess_bulk = if (is_stanfit && always_diagnose) ess_bulk(cell), 
                                                                                ess_tail = if (is_stanfit && always_diagnose) ess_tail(cell),
                                                                                rhat = if (is_stanfit && always_diagnose) Rhat(cell),)) %>%
        rename(variable = parameters) %>% 
        mutate(iter_data = map(iter_data, ~ tibble(iter_est = c(.), iter_id = seq(nrow(.) * ncol(.)))))
      } else if (is_tibble(.)) {
        filter(., str_detect(variable, r"{^cluster_cf_cutoff\[.+\]$}"))
      } else { 
        .$draws("cluster_cf_cutoff") %>% 
          posterior::as_draws_df() %>% 
          mutate(iter_id = .draw) %>% 
          pivot_longer(!c(iter_id, .draw, .iteration, .chain), names_to = "variable", values_to = "iter_est") %>% 
          nest(iter_data = !variable)
      }
  } %>%   
    tidyr::extract(variable, c("treatment_index", "mu_treatment_index", "cluster_id"), r"{(\d+),(\d+)(?:,(\d+))?}", convert = TRUE) %>%  
    mutate(
      treatment_index_obs = stan_data$cluster_assigned_dist_group_treatment[cluster_id],
      assigned_dist_standard_obs = stan_data$cluster_standard_dist[cluster_id],
      assigned_dist_obs = unstandardize(assigned_dist_standard_obs, analysis_data$cluster.dist.to.pot),
      mu_assigned_treatment = factor(mu_treatment_index, levels = 1:4, labels = levels(stan_data$cluster_assigned_treatment)), 
    ) %>% 
    left_join(cluster_treatment_map, by = "treatment_index") %>% 
    left_join(cluster_treatment_map, by = c("treatment_index_obs" = "treatment_index"), suffix = c("", "_obs")) %>% 
    left_join(stan_data$analysis_data %>% count(cluster_id, name = "cluster_size"), by = "cluster_id") %>% 
    left_join(
      stan_data$analysis_data %>%
        select(cluster_id, assigned_treatment = assigned.treatment, assigned_dist_group = dist.pot.group, {{ dewormed_var }}) %>%
        group_by(cluster_id, assigned_treatment, assigned_dist_group) %>%
        summarize(obs_num_takeup = sum({{ dewormed_var }}), .groups = "drop"),
      by = c("cluster_id", "assigned_treatment", "assigned_dist_group")
    ) %>%
    mutate(
      # obs_num_takeup = if_else(is.na(mu_assigned_treatment) | treatment_index == mu_treatment_index, obs_num_takeup, NA_integer_)
      obs_num_takeup = if_else(fct_match(model_type, "reduced form") | is.na(mu_assigned_treatment) | assigned_treatment == mu_assigned_treatment, obs_num_takeup, NA_integer_),
      obs_prop_takeup = obs_num_takeup / cluster_size
    ) %>% 
    as_tibble()
}


extract_obs_cf <- function(fit, par, stan_data, iter_level = c("obs", "cluster"), quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95), thin = 1, dewormed_var = dewormed, always_diagnose = TRUE) {
  analysis_data <- stan_data$analysis_data
  
  iter_level <- rlang::arg_match(iter_level)
  
  obs_index_col <- switch(iter_level, obs = "obs_index", cluster = "cluster_id")
  
  cluster_treatment_map <- stan_data$cluster_treatment_map %>% 
    mutate(treatment_index = seq(n()))
  
  is_stanfit <- is(fit, "stanfit")
  
  fit %>% {
    if (is_stanfit) {
      as.array(., par = par) 
    } else {
      .$draws(par)
    }
  } %>% {
      if (thin > 1) magrittr::extract(., (seq_len(nrow(.)) %% thin) == 0,,) else .
    } %>%  
    plyr::adply(3, function(cell) tibble(iter_data = list(cell)) %>% mutate(ess_bulk = if (is_stanfit && (thin > 1 || always_diagnose)) ess_bulk(cell), 
                                                                            ess_tail = if (is_stanfit && (thin > 1 || always_diagnose)) ess_tail(cell),
                                                                            rhat = if (is_stanfit && (thin > 1 || always_diagnose)) Rhat(cell),)) %>% {
      if (is_stanfit) rename(., variable = parameters) else .
    } %>% 
    tidyr::extract(variable, c("treatment_index", obs_index_col), "(\\d+),(\\d+)", convert = TRUE) %>%  
    mutate(
      iter_data = map(iter_data, ~ tibble(iter_est = c(.), iter_id = seq(nrow(.) * ncol(.)))),
      cluster_id = switch(iter_level,
                          cluster = cluster_id, 
                          obs = stan_data$obs_cluster_id[!!sym(obs_index_col)]),
      treatment_index_obs = stan_data$cluster_assigned_dist_group_treatment[cluster_id],
      assigned_dist_standard_obs = stan_data$cluster_standard_dist[cluster_id],
      assigned_dist_obs = unstandardize(assigned_dist_standard_obs, analysis_data$cluster.dist.to.pot),
    ) %>% 
    left_join(cluster_treatment_map, by = "treatment_index") %>% 
    left_join(cluster_treatment_map, by = c("treatment_index_obs" = "treatment_index"), suffix = c("", "_obs")) %>% 
    left_join(stan_data$analysis_data %>% count(cluster_id, name = "cluster_size"), by = "cluster_id") %>% 
    left_join(
      stan_data$analysis_data %>%
        select(cluster_id, assigned_treatment = assigned.treatment, assigned_dist_group = dist.pot.group, {{ dewormed_var }}) %>%
        group_by(cluster_id, assigned_treatment, assigned_dist_group) %>%
        summarize(obs_num_takeup = sum({{ dewormed_var }})) %>%
        ungroup(),
      by = c("cluster_id", "assigned_treatment", "assigned_dist_group")
    ) %>% 
    as_tibble()
}

extract_obs_fit_level <- function(fit, par, stan_data, iter_level = c("obs", "cluster", "none"), by_treatment = FALSE, mix = FALSE, summarize_est = TRUE, quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95)) {
  analysis_data <- stan_data$analysis_data
  
  iter_level <- rlang::arg_match(iter_level)
  
  obs_index_col <- if (iter_level != "none") "obs_index"
  
  if (by_treatment) obs_index_col %<>% c("treatment_index") 
  if (mix) obs_index_col %<>% c("mix_index", .) 
  
  is_stanfit <- is(fit, "stanfit")
  
  fit_data <- tryCatch({
    fit_data <- if (is_stanfit) {
      fit %>% 
        as.array(par = par) %>% 
        plyr::adply(3, function(cell) tibble(iter_data = list(cell)) %>% mutate(ess_bulk = ess_bulk(cell), 
                                                                                ess_tail = ess_tail(cell),
                                                                                rhat = Rhat(cell),)) %>% 
        mutate(iter_data = map(iter_data, ~ tibble(iter_est = c(.), iter_id = seq(nrow(.) * ncol(.))))) %>%
        as_tibble()
    } else if (is_tibble(fit)) {
      fit %>% 
        filter(str_detect(variable, str_glue(r"{^{par}\[.+\]$}")))
    } else { 
      fit$draws(par) %>% 
        posterior::as_draws_df() %>% 
        mutate(iter_id = .draw) %>% 
        pivot_longer(!c(iter_id, .draw, .iteration, .chain), names_to = "variable", values_to = "iter_est") %>% 
        nest(iter_data = !variable)
    }
  }, error = function(err) NULL)
  
  if (is_null(fit_data) || nrow(fit_data) == 0) return(NULL) 
  
  if (iter_level == "none" && !by_treatment) {
    if (is_stanfit) {
      fit_data %>% 
        select(-parameters) 
    } else {
      fit_data %>% 
        select(-variable)
    }
  } else {
    extracted <- fit_data %>% {
      if (is_stanfit) rename(., variable = parameters) else .
    } %>% 
      tidyr::extract(variable, 
                     obs_index_col, 
                     str_c(rep_along(obs_index_col, r"{(\d+)}"), collapse = ","),
                     convert = TRUE)
    
    if (summarize_est) {
      extracted %<>% 
        mutate(mean_est = map_dbl(iter_data, ~ mean(.$iter_est, na.rm = TRUE)),
               quantiles_est = map(iter_data, quantilize_est, 
                                   iter_est,
                                   quant_probs = quant_probs,
                                   na.rm = TRUE))
    }
    
    if (by_treatment) {
      extracted %<>% 
        left_join(mutate(stan_data$cluster_treatment_map, treatment_index = seq(n())), by = "treatment_index") %>% 
        mutate(
          # assigned_treatment = factor(treatment_index, levels = 1:4, labels = levels(stan_data$cluster_assigned_treatment)), 
          assigned_dist_standard = NULL,
          assigned_dist = NULL 
        )
        
    } else {
      extracted %<>% 
        mutate(
          assigned_treatment = switch(iter_level,
                                      cluster = stan_data$cluster_assigned_treatment,
                                      obs = stan_data$assigned_treatment),
          assigned_dist_standard = switch(iter_level,
                                          cluster = stan_data$cluster_standard_dist,
                                          obs = stan_data$standard_dist),
          assigned_dist = switch(iter_level,
                                 cluster = unstandardize(assigned_dist_standard, analysis_data$cluster.dist.to.pot),
                                 obs = unstandardize(assigned_dist_standard, analysis_data$cluster.dist.to.pot)),
        )
    }
    
    return(extracted)
  }
}

extract_sim_diff <- function(level, quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95)) {
  level %>% 
    select(-mean_est, -quantiles_est) %>% 
    rename(assigned_treatment_left = assigned_treatment) %>% 
    mutate(assigned_treatment_right = "control") %>% 
    left_join(select(., -grid_dist, -assigned_treatment_right), 
              by = c("assigned_treatment_right" = "assigned_treatment_left", "grid_index"), 
              suffix = c("_left", "_right")) %>% 
    filter(assigned_treatment_left != assigned_treatment_right) %>% 
    mutate(iter_diff = map2(iter_data_left, iter_data_right, inner_join, by = "iter_id", suffix = c("_left", "_right")) %>% 
             map(mutate, iter_est = iter_est_left - iter_est_right)) %>% 
    mutate(mean_est = map_dbl(iter_diff, ~ mean(.$iter_est)),
           quantiles_est = map(iter_diff, quantilize_est, iter_est, 
                                       probs = quant_probs)) 
}

extract_roc_param <- function(fit, stan_data, par, quant_probs = c(0.05, 0.1, 0.5, 0.9, 0.95)) {
  analysis_data <- stan_data$analysis_data
  
  fit_data <- tryCatch({
    if (is_tibble(fit)) {
      fit %>% 
        filter(str_detect(variable, str_glue(r"{^{par}\[.+\]$}")))
    } else { 
      fit$draws(par) %>% 
        posterior::as_draws_df() %>% 
        mutate(iter_id = .draw) %>% 
        pivot_longer(!c(iter_id, .draw, .iteration, .chain), names_to = "variable", values_to = "iter_est") %>% 
        nest(iter_data = !variable)
    }
  }, error = function(err) NULL)
  
  if (is_null(fit_data) || nrow(fit_data) == 0) return(NULL)
  
  fit_data %>% 
    tidyr::extract(variable, c("cluster_id", "roc_distance_index"), r"{(\d+),(\d+)}", convert = TRUE) %>%  
    left_join(tibble(roc_distance = unstandardize(stan_data$roc_distances, analysis_data$cluster.dist.to.pot)) %>% 
                mutate(roc_distance_index = seq(n())), by = "roc_distance_index") %>% 
    left_join(stan_data$analysis_data %>% count(cluster_id, name = "cluster_size"), by = "cluster_id") %>% 
    as_tibble()
}

generate_initializer <- function(num_treatments,
                                 num_clusters,
                                 num_counties,
                                 base_init = function() lst(beta_cluster_sd = abs(rnorm(num_treatments))), 
                                 structural_type = 0,
                                 num_mix = 1,
                                 num_dist_mix = 1,
                                 use_cluster_effects = use_cluster_effects,
                                 use_county_effects = use_cluster_effects,
                                 use_param_dist_cluster_effects = use_cluster_effects,
                                 use_mu_cluster_effects = use_cluster_effects,
                                 use_mu_county_effects = FALSE,
                                 use_single_cost_model = FALSE,
                                 restricted_private_incentive = FALSE,
                                 cost_model_type = NA,
                                 num_knots = NA,
                                 name_matched = FALSE,
                                 suppress_reputation = FALSE) {
  base_list <- base_init()
  
  if (structural_type > 0 || !is_empty(base_list)) {
    function() {
      if (structural_type > 0) {
        num_beta_param <- if (restricted_private_incentive) num_treatments - 1 else num_treatments
        
        salience <- cost_model_type %in% cost_model_types[c("param_linear_salience", "param_quadratic_salience", "semiparam_salience")] 
        param_kappa <- cost_model_type == cost_model_types["param_kappa"]
        discrete_cost <- cost_model_type == cost_model_types["discrete"]
        linear <- !param_kappa && !discrete_cost
        quadratic <- cost_model_type %in% cost_model_types[c("param_quadratic", "param_quadratic_salience")] 
        semiparam <- cost_model_type %in% cost_model_types[c("semiparam", "semiparam_salience")] 
        
        num_discrete_dist <- if (cost_model_type %in% cost_model_types["discrete"]) 2 else 1
        num_incentive_treatments <- num_treatments
        num_treatments <- if (cost_model_type %in% cost_model_types["discrete"]) num_treatments * num_discrete_dist else num_treatments
        
        init <- lst(
          mu_rep_raw = if (suppress_reputation && !salience) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)),
          mu_rep = if (suppress_reputation && !salience) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)),
          v_mu = rnorm(1, 0, 0.1),
          beta_control = rnorm(num_discrete_dist, 0, 0.1) %>% { if (num_discrete_dist > 1) as.array(.) else . }, 
          beta_ink_effect = rnorm(num_discrete_dist, 0, 0.1) %>% { if (num_discrete_dist > 1) as.array(.) else . },
          beta_calendar_effect = { 
            if (restricted_private_incentive) abs(rnorm(num_discrete_dist, 0, 0.1)) else rnorm(num_discrete_dist, 0, 0.1) 
          } %>% { if (num_discrete_dist > 1) as.array(.) else . },
          beta_bracelet_effect = { 
            if (restricted_private_incentive) abs(rnorm(num_discrete_dist, 0, 0.1)) else rnorm(num_discrete_dist, 0, 0.1)
          } %>% { if (num_discrete_dist > 1) as.array(.) else . },
          beta_salience = abs(rnorm(1, 0, 0.1)),
          dist_beta_salience = abs(rnorm(1, 0, 0.1)),
          dist_quadratic_beta_salience = abs(rnorm(1, 0, 0.1)),
          beta_nm_effect = if (name_matched) rnorm(num_treatments, 0, 0.1) else array(dim = 0),
          dist_cost_k_raw = if (!param_kappa) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)),
          dist_cost_k = if (!param_kappa) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)),
          dist_beta_v = if (!linear) array(dim = 0) else if (salience || use_single_cost_model) as.array(abs(rnorm(1, 0, 0.1))) else rnorm(num_treatments, 0, 0.1), 
          dist_quadratic_beta_v = if (!quadratic) array(dim = 0) else if (salience || use_single_cost_model) as.array(abs(rnorm(1, 0, 0.1))) else abs(rnorm(num_treatments, 0, 0.1)), 
          u_splines_v_raw = if (!semiparam) {
            array(dim = c(0, num_knots)) 
          } else {
            num_semiparam_treatments <- if (salience || use_single_cost_model) 1 else num_treatments
            array(rnorm(num_semiparam_treatments * num_knots, 0, 0.1), dim = c(num_semiparam_treatments, num_knots))
          },
          u_splines_v = u_splines_v_raw,
         
          u_sd = abs(rnorm(num_treatments, 0, 0.5)),
          
          structural_beta_cluster = if (use_cluster_effects) matrix(rnorm(num_clusters * num_treatments, 0, 0.1), nrow = num_clusters, ncol = num_treatments) else array(dim = 0),
          structural_beta_cluster_raw = if (use_cluster_effects) matrix(rnorm(num_clusters * num_treatments, 0, 0.1), nrow = num_clusters, ncol = num_treatments) else array(dim = c(0, num_treatments)),
          structural_beta_cluster_sd = if (use_cluster_effects) abs(rnorm(num_treatments, 0, 0.1)) else array(dim = 0),
          
          structural_beta_county = if (use_county_effects) matrix(rnorm(num_counties * num_treatments, 0, 0.1), nrow = num_counties, ncol = num_treatments) else array(dim = 0),
          structural_beta_county_raw = if (use_county_effects) matrix(rnorm(num_counties * num_treatments, 0, 0.1), nrow = num_counties, ncol = num_treatments) else array(dim = c(0, num_treatments)),
          structural_beta_county_sd = if (use_county_effects) abs(rnorm(num_treatments, 0, 0.1)) else array(dim = 0),
          
          beta_nm_effect_cluster = if (use_cluster_effects && name_matched) matrix(rnorm(num_clusters * num_treatments, 0, 0.1), nrow = num_clusters, ncol = num_treatments) else array(dim = c(0, num_treatments)),
          beta_nm_effect_cluster_raw = if (use_cluster_effects && name_matched) matrix(rnorm(num_clusters * num_treatments, 0, 0.1), nrow = num_clusters, ncol = num_treatments) else array(dim = c(0, num_treatments)),
          beta_nm_effect_cluster_sd = if (use_cluster_effects && name_matched) abs(rnorm(num_treatments, 0, 0.1)) else array(dim = 0),
          
          structural_cluster_takeup_prob = matrix(rbeta(num_clusters * num_mix, 10, 10), nrow = num_mix),
          
          dist_beta_cluster_raw = if (discrete_cost) {
            array(0, dim = c(0, 0))
          } else if (!use_param_dist_cluster_effects) { 
            if (salience || use_single_cost_model) array(dim = c(0, 1)) else array(dim = c(0, num_treatments)) 
          } else {
            if (salience || use_single_cost_model) matrix(rnorm(num_clusters, 0, 0.1), nrow = num_clusters, ncol = 1)
            else matrix(rnorm(num_clusters * num_treatments, 0, 0.1), nrow = num_clusters, ncol = num_treatments)
          },
          dist_beta_cluster_sd = if (!use_param_dist_cluster_effects) array(dim = 0) 
            else if (salience || use_single_cost_model) as.array(abs(rnorm(1, 0, 0.1)))
            else abs(rnorm(num_treatments, 0, 0.1)),
          mu_cluster_effects_raw = if (!use_mu_cluster_effects || (suppress_reputation && !salience)) array(dim = c(0, num_incentive_treatments)) else matrix(rnorm(num_clusters * num_treatments, 0, 0.1), num_clusters, num_treatments),
          mu_cluster_effects_sd = if (!use_mu_cluster_effects || (suppress_reputation && !salience)) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)) ,
          mu_county_effects_raw = if (!use_mu_cluster_effects || (suppress_reputation && !salience)) array(dim = c(0, num_incentive_treatments)) else matrix(rnorm(num_counties * num_treatments, 0, 0.1), num_counties, num_treatments) ,
          mu_county_effects_sd = if (!use_mu_cluster_effects || (suppress_reputation && !salience)) array(dim = 0) else abs(rnorm(num_treatments, 0, 0.1)),
          cluster_mu_rep = if (!use_mu_cluster_effects || (suppress_reputation && !salience)) array(0, num_treatments) else matrix(mu_rep, num_clusters, num_treatments, byrow = TRUE),
          lambda_v_mix = rep(1 / num_mix, num_mix),
          v_mix_mean = as.array(0.1),
          v_sd = rbeta(1, 8, 1),
          
          group_dist_mix = MCMCpack::rdirichlet(2, rep(10, num_dist_mix))
        )
        
        base_list %>% 
          list_modify(!!!init)
                      
      } else {
        return(base_list) 
      }
    }
  } else return(NULL)
}

fit_model <- function(curr_stan_data, chains, threads, iter, use_cmdstanr, include_paths) {
  control <- lst(adapt_delta = 0.8) 
  
  if (!is_null(curr_stan_data$control)) {
    control %<>% list_modify(!!!curr_stan_data$control)
    curr_stan_data$control <- NULL
  } 
  
  if (use_cmdstanr) {
    dist_model <- cmdstan_model(
      file.path("stan_models", curr_stan_data$model_file),
      cpp_options = list(stan_threads = TRUE),
      include_paths = include_paths
    )
    
    dist_model$sample(
      data = curr_stan_data %>% 
        discard(~ is_function(.x) | is.character(.)) %>% 
        list_modify(analysis_data = NULL),
      chains = chains,
      parallel_chains = chains,
      threads_per_chain = threads,
      iter_warmup = iter %/% 2, 
      iter_sampling = iter %/% 2, 
      save_warmup = FALSE,
      thin = (curr_stan_data$thin %||% 1),
      init = curr_stan_data$init,
      adapt_delta = control$adapt_delta,
      max_treedepth = control$max_treedepth
    )
  } else { 
    stan_model(file.path("stan_models", curr_stan_data$model_file)) %>% 
      sampling(
        iter = iter, 
        thin = (curr_stan_data$thin %||% 1),
        chains = chains,
        control = control,
        save_warmup = FALSE,
        pars = curr_stan_data$pars,
        init = curr_stan_data$init %||% "random",
        data = curr_stan_data
      )
  }
} 

stan_list <- function(models_info, stan_data, script_options, use_cmdstanr = FALSE, include_paths = ".") {
  get_sample_file_name <- . %>% 
    sprintf("dist_model_%s.csv") %>% 
    file.path("stanfit", .) 
  
  if (script_options$fit || script_options$prior) {
    inner_sampler <- function(curr_model, stan_data) {
      curr_stan_data <- stan_data %>%
        map_at(c("cluster_treatment_map", "beliefs_ate_pairs"), ~ mutate(.x, across(.fns = as.integer)) %>% as.matrix()) %>%  # A tibble of factors no longer gets converted into an "array[,] int" in Stan.
        list_modify(!!!curr_model) %>%
        map_if(is.factor, as.integer)
        
      iter <- if (script_options$force_iter) iter else (curr_stan_data$iter %||% iter)
      
      fit_model(curr_stan_data, script_options$chains, script_options$threads, iter, use_cmdstanr, include_paths)
    }
    
    if (script_options$sequential) {
      models_info %>% 
        map(inner_sampler,
            stan_data = stan_data)
      
    } else {
      models_info %>% 
        pbmclapply(inner_sampler,
                   stan_data = stan_data,
                   ignore.interactive = TRUE,
                   mc.silent = TRUE,
                   mc.cores = 3)
    }
  } else if (script_options$cv) {
    models_info %>% 
      imap(function(curr_model, model_name, stan_data, use_cmdstanr) {
        kfold_groups <- kfold_split_stratified(K = folds, x = stan_data$cluster_assigned_dist_group_treatment)
        
        dist_model <- if (use_cmdstanr) {
          cmdstan_model(file.path("stan_models", curr_model$model_file), include_paths = include_paths)
        } else {
          stan_model(file.path("stan_models", curr_model$model_file))
        }
        
        log_lik_list <- map(seq(folds), ~ which(kfold_groups == .)) %>% 
          pbmclapply(function(excluded_clusters, model_name, dist_model, stan_data, use_cmdstanr) {
            curr_stan_data <- stan_data %>%
              list_modify(!!!curr_model,
                          excluded_clusters = excluded_clusters,
                          num_excluded_clusters = length(excluded_clusters)) %>%
              map_if(is.factor, as.integer)
            
            curr_iter <- if (script_options$force_iter) iter else (curr_stan_data$iter %||% iter)
            curr_chains <- chains
            
            if (use_cmdstanr) {
              fit <- dist_model$sample(
                data = curr_stan_data,
                iter_warmup = curr_iter %/% 2,
                iter_sampling = curr_iter %/% 2,
                save_warmup = FALSE,
                thin = thin_by,
                chains = curr_chains,
                parallel_chains = chains,
                adapt_delta = 0.9,,
                init = curr_model$init
              )
              
              fit$draws("log_lik_heldout")
            } else {
              sampling(dist_model,
                       iter = curr_iter,
                       thin = thin_by,
                       chains = curr_chains,
                       control = lst(adapt_delta = 0.9),
                       save_warmup = FALSE,
                       init = curr_model$init %||% "random",
                       pars = "log_lik_heldout", 
                       data = curr_stan_data) %>% 
                extract_log_lik("log_lik_heldout") 
            }
          },
          dist_model = dist_model,
          stan_data = stan_data,
          use_cmdstanr = use_cmdstanr,
          model_name = model_name,
          ignore.interactive = TRUE,
          mc.silent = !script_options$sequential,
          mc.cores = if (script_options$sequential) 1 else 3)
          
        return(tryCatch(kfold(log_lik_list), 
                        error = function(err) { 
                          print(err)
                          return(log_lik_list)
                        }))
      },
      stan_data = stan_data, use_cmdstanr = use_cmdstanr) 
  }
}

create_stan_enum <- function(enum_names, prefix) {
  enum_names %>% 
    set_names(seq(length(.)), .) %>% 
    structure(prefix = prefix, class = "stan_enum") 
}

enum2stan <- function(x) UseMethod("enum2stan")
enum2stan.stan_enum <- function(enum) { 
  upper_prefix <- str_to_upper(attr(enum, 'prefix'))
  min_const_name <- str_glue("MIN_{upper_prefix}_VALUE")
  max_const_name <- str_glue("MAX_{upper_prefix}_VALUE")
  values <- str_glue("int<lower = {min_const_name}, upper = {max_const_name}> {upper_prefix}_{str_to_upper(names(enum))};")
  
  print(
    str_glue("int {min_const_name};"),
    str_glue("int {max_const_name};"),
    values)
}

enum2stan_data <- function(x) UseMethod("enum2stan_data")
enum2stan_data.stan_enum <- function(enum) { 
  upper_prefix <- str_to_upper(attr(enum, 'prefix'))
  min_const_name <- str_glue("MIN_{upper_prefix}_VALUE")
  max_const_name <- str_glue("MAX_{upper_prefix}_VALUE")
  
  as.list(enum) %>% 
    set_names(str_c(upper_prefix, "_", str_to_upper(names(enum)))) %>% 
    list_modify(!!min_const_name := min(enum),
                !!max_const_name := max(enum))
}

rep_normal <- function(v, ...) dnorm(v, ...) / ((pnorm(v, ...) * pnorm(v, ..., lower.tail = FALSE)))

integrate_delta_part <- function(w, u_sd) { 
  delta_part <- function(v, w, u_sd) v * pnorm(w - v, sd = u_sd) * dnorm(v)
  delta_part_integrator <- function(w, u_sd) integrate(delta_part, w = w, u_sd = u_sd, -Inf, Inf)$value
  vec_delta_part_integrator <- Vectorize(delta_part_integrator, vectorize.args = c("w", "u_sd")) 
  
  vec_delta_part_integrator(w, u_sd)
}

calculate_delta <- function(w, total_error_sd, u_sd) {
  F_w <- pnorm(w, sd = total_error_sd)
  r <- if (u_sd > 0) {
    integrate_delta_part(w, u_sd)
  } else {
    - dnorm(w)  
  }
  
  return(- r / (F_w * (1 - F_w)))
} 


generate_v_cutoff_fixedpoint <- function(b, mu, total_error_sd = 1, u_sd = 0) {
  function(v_cutoff) {
    v_cutoff + b + mu * calculate_delta(v_cutoff, total_error_sd, u_sd)
  }
}

rep_normal_1std <- function(v, ...) {
  phi_v <- dnorm(v, ...)
  Phi_1mPhi_v <- ((pnorm(v, ...) * pnorm(v, ..., lower.tail = FALSE)))
  
  (phi_v / Phi_1mPhi_v^2) * (-(v * Phi_1mPhi_v) + (phi_v * (2 * pnorm(v, ...) - 1))) 
}

calculate_delta_deriv <- function(w, total_error_sd, u_sd) {
  if (all(u_sd > 0)) {
    delta_part_deriv <- function(v, w, u_sd) v * dnorm(w - v, sd = u_sd) * dnorm(v)
    
    F_w <- pnorm(w, sd = total_error_sd)
   
    int_part <- integrate_delta_part(w, u_sd)
    int_part_deriv <- Vectorize(function(w, u_sd) integrate(delta_part_deriv, w = w, u_sd = u_sd, -Inf, Inf)$value, vectorize.args = c("w", "u_sd"))(w, u_sd)  
    
    return((- int_part_deriv * F_w * (1 - F_w) + int_part * dnorm(w, sd = total_error_sd) * (1 - 2 * F_w))/((F_w * (1 - F_w))^2))
  } else {
    return(rep_normal_1std(w))
  }
}

social_multiplier <- function(v, mu, total_error_sd, u_sd) {
  - 1 / (1 + mu * calculate_delta_deriv(v, total_error_sd, u_sd))
}

expect_y_partial_bbar <- function(v, mu, total_error_sd, u_sd) {
  - dnorm(v, sd = total_error_sd) * social_multiplier(v, mu, total_error_sd, u_sd) 
}

expect_y_partial_c <- function(v, mu, total_error_sd, u_sd) {
  dnorm(v, sd = total_error_sd) * social_multiplier(v, mu, total_error_sd, u_sd) 
}

expect_y_partial_d <- function(v, mu, total_error_sd, u_sd, linear_dist_cost) {
  expect_y_partial_c(v, mu, total_error_sd, u_sd) %>% 
    multiply_by(map_dbl(linear_dist_cost, ~ weighted.mean(.x$iter_est, .x$cluster_size))) 
}

get_wtp_results <- function(wtp_draws) {
  wtp_draws %>% 
    filter(str_detect(variable, r"{^(prob_prefer_calendar|(strata|hyper)_wtp_mu)}")) %>% 
    tidyr::extract(variable, c("variable", "index"), r"{([^\[]+)(?:\[(\d+)\])?}", convert = TRUE) %>% 
    mutate(
      quants = map(iter_data, quantilize_est, iter_est, quant_probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE),
      mean_est = map_dbl(iter_data, ~ mean(.x$iter_value, na.rm = TRUE)),
    ) %>% 
    unnest(quants)
}

get_beliefs_results <- function(beliefs_draws, stan_data) {
  lst(
    prob_knows = beliefs_draws %>% 
      filter(str_detect(variable, r"{^(prob_1ord|prob_2ord)\[}")) %>% 
      tidyr::extract(variable, c("ord", "treatment_id"), r"{([12])ord\[(\d+)\]}", convert = TRUE) %>% 
      mutate(quants = map(iter_data, quantilize_est, iter_est, quant_probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)) %>% 
      unnest(quants) %>% 
      right_join(mutate(stan_data$cluster_treatment_map, treatment_id = seq(n())), ., by = "treatment_id") %>% 
      arrange(ord),
    
    ate_knows = beliefs_draws %>% 
      filter(str_detect(variable, r"{^(ate_1ord|ate_2ord)\[}")) %>% 
      tidyr::extract(variable, c("ord", "ate_pair_index"), r"{([12])ord\[(\d+)\]}", convert = TRUE) %>% 
      mutate(quants = map(iter_data, quantilize_est, iter_est, quant_probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)) %>% 
      unnest(quants) %>% 
      right_join(mutate(stan_data$beliefs_ate_pairs, ate_pair_index = seq(n())), ., by = "ate_pair_index") %>% 
      right_join(mutate(stan_data$cluster_treatment_map, treatment_id = seq(n())), ., by = c("treatment_id")) %>%
      right_join(mutate(stan_data$cluster_treatment_map, treatment_id = seq(n())), ., by = c("treatment_id" = "treatment_id_control"), suffix = c("_right", "_left")) %>%
      select(-starts_with("treatment_id"), -ate_pair_index) %>% 
      arrange(ord)
  )
}

get_dist_results <- function(fit, stan_data = NULL, model_type = "structural") {
  if (!is_null(fit) && fct_match(model_type, "structural")) {
    temp_res <- if (is(fit, "stanfit")) {
      as.data.frame(fit, pars = c("group_dist_mean", "group_dist_sd", "group_dist_mix")) %>% 
        # sample_n(min(nrow(.), 1500)) %>% 
        mutate(iter_id = seq(n())) %>% 
        pivot_longer(names_to = c(".value", "assigned_dist_group", "mix_index"), names_pattern = "([^\\[]+)\\[(\\d),(\\d)", cols = -iter_id, values_drop_na = TRUE) 
    } else if (is_tibble(fit)) {
      filter(fit, str_detect(variable, str_c("^", str_c(c("group_dist_mean", "group_dist_sd", "group_dist_mix"), collapse = "|"), "\\["))) %>% 
        unnest(iter_data) %>% 
        # filter(iter_id %in% sample(max(iter_id), 1500)) %>% 
        select(!c(.chain, .iteration, .draw)) %>% 
        tidyr::extract(variable, c("variable", "assigned_dist_group", "mix_index"), r"{(\w+)\[(\d),(\d)}", convert = TRUE) %>% 
        pivot_wider(c(assigned_dist_group, mix_index, iter_id), names_from = variable, values_from = iter_est)
    } else {
      fit$draws(c("group_dist_mean", "group_dist_sd", "group_dist_mix")) %>% 
        posterior::as_draws_df() %>% 
        # sample_n(min(nrow(.), 1500)) %>% 
        mutate(iter_id = seq(n())) %>% 
        pivot_longer(names_to = c(".value", "assigned_dist_group", "mix_index"), names_pattern = "([^\\[]+)\\[(\\d),(\\d)", cols = -iter_id, values_drop_na = TRUE) 
    }
   
    if (!is_null(stan_data)) {
      dist_levels <- levels(stan_data$cluster_treatment_map$assigned_dist_group) 
      
      temp_res %<>% 
        mutate(assigned_dist_group = factor(assigned_dist_group, levels = 1:2, labels = dist_levels))
    }
    
    return(temp_res)
  }
}

get_imputed_dist <- function(fit, stan_data, model_type = "structural") {
  if (!is_null(fit) && fct_match(model_type, "structural")) {
    temp_res <- if (is(fit, "stanfit")) {
      stop("not supported")
    } else if (is_tibble(fit)) {
      filter(fit, str_detect(variable, "missing_cluster_standard_dist")) 
    } else {
      fit$draws(c("missing_cluster_standard_dist")) %>% 
        posterior::as_draws_df() %>% 
        mutate(iter_id = seq(n())) %>% 
        select(!c(.chain, .iteration, .draw)) %>% 
        pivot_longer(names_to = "variable", values_to = "iter_est", cols = -iter_id) %>% 
        nest(iter_data = c(iter_id, iter_est)) 
    }
    
    temp_res %>%  
      tidyr::extract(variable, c("cluster_id", "dist_treatment_id"), r"{(\d+),(\d+)}", convert = TRUE) %>% 
      mutate(dist_treatment = factor(dist_treatment_id, levels = 1:2, labels = c("close", "far"))) %>% 
      left_join(
        stan_data$analysis_data %>% 
          distinct(county, cluster_id, dist.pot.group, obs_standard_dist = standard_cluster.dist.to.pot),
        by = c("cluster_id", "dist_treatment" = "dist.pot.group")
      ) %>% 
      mutate(quants = map(iter_data, quantilize_est, iter_est, na.rm = TRUE)) %>% 
      unnest(quants)
  }
}

# Plotting Functions ------------------------------------------------------

plot_estimands <- function(.data, y, results_group = model, group_labels = NULL, include_prior_predict = FALSE, pos_height = 0.8, center_bar_size = 3) {
  plot_pos <- ggstance::position_dodgev(height = pos_height)
  
  ggplot_obj <- if (include_prior_predict) {
    ggplot(.data, aes(x = per_0.5, y = {{ y }}, group = model)) +
      geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, group = model), alpha = 0.15, fatten = 3, size = 10, position = plot_pos, data = . %>% filter(fct_match(fit_type, "prior-predict"))) +
      geom_linerange(aes(xmin = per_0.05, xmax = per_0.9, group = model), alpha = 0.1, fatten = 3, size = 6, position = plot_pos, data = . %>% filter(fct_match(fit_type, "prior-predict"))) +
      geom_linerange(aes(xmin = per_0.05, xmax = per_0.5, group = model), alpha = 0.1, fatten = 3, size = 4, position = plot_pos, data = . %>% filter(fct_match(fit_type, "prior-predict"))) +
      NULL
  } else {
    ggplot(.data, aes(x = per_0.5, y = {{ y }}, group = {{ results_group }})) 
  }
  
  ggplot_obj +
    geom_linerange(aes(xmin = per_0.25, xmax = per_0.75, color = {{ results_group }}), alpha = 0.4, size = center_bar_size, position = plot_pos) +
    geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9, color = {{ results_group }}), fatten = 0, size = 0.4, width = 0.5, position = plot_pos) +
    geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, color = {{ results_group }}), size = 0.4, position = plot_pos) +
    
    geom_point(aes(x = mean_est, color = {{ results_group }}), position = plot_pos) + 
    geom_point(aes(x = mean_est), color = "white", size = 0.75, position = plot_pos) + 
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_color_manual("", 
                       values = select(dist_fit_data, {{ results_group }}, model_color) %>% deframe(), 
                       labels = if (is_null(group_labels)) { 
                         dist_fit_data %>% 
                           select(model, model_name) %>% 
                           deframe() 
                       } else {
                         group_labels
                       }, aesthetics = c("color", "fill")) + 
    labs(
      caption = #"Dotted line range: 98% credible interval. 
                "Line range: 90% credible interval. 
                 Outer box: 80% credible interval. Inner box: 50% credible interval. 
                 Thick vertical line: median. Point: mean."
      
    ) +
    theme(legend.position = "top", legend.direction = "vertical") +
    guides(color = guide_legend(ncol = 3)) +
    NULL
}

plot_estimand_hist <- function(.data, x, binwidth = NULL, results_group = model, group_labels = NULL) {
  .data %>% 
    ggplot(aes(group = {{ results_group }})) + 
    geom_histogram(aes(x = {{ x }}, y = stat(density) * (binwidth %||% 1), fill = {{ results_group }}), 
                   binwidth = binwidth, boundary = 0, position = "identity", alpha = 0.5,  
                   data = . %>% unnest(iter_data)) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_color_manual("", 
                       values = select(dist_fit_data, {{ results_group }}, model_color) %>% deframe(), 
                       labels = if (is_null(group_labels)) { 
                         dist_fit_data %>% 
                           select(model, model_name) %>% 
                           deframe() 
                       } else {
                         group_labels
                       }, aesthetics = c("color", "fill")) + 
    theme(legend.position = "top", legend.direction = "vertical") +
    guides(color = guide_legend(ncol = 3)) +
    NULL
}


plot_wtp_results <- function(draws) {
  cowplot::plot_grid(
    filter(draws, fct_match(variable, "prob_prefer_calendar")) %>% 
      left_join(tibble(index = 1:21, val_diff = -seq(-100, 100, 10)), by = "index") %>% 
      ggplot(aes(x = val_diff)) +
      geom_line(aes(y = per_0.5)) +
      geom_point(aes(y = per_0.5), size = 1) +
      geom_ribbon(aes(ymin = per_0.1, ymax = per_0.9), alpha = 0.4) +
      scale_x_continuous("Added Value [KSh]", breaks = seq(-100, 100, 10)) + #, labels = scales::label_number(suffix = " KSh")) +
      labs(
        title = "Probability of Preference for Calendars vs. Bracelets",
        y = "" 
      ) +
      NULL,
    
    # filter(draws, fct_match(variable, "strata_wtp_mu")) %>%
    filter(draws, fct_match(variable, "hyper_wtp_mu")) %>%
      mutate(across(c(starts_with("per_"), mean_est), multiply_by, 100)) %>%
      # ggplot(aes(y = str_c(variable, index))) +
      ggplot(aes(y = str_c(variable))) +
      geom_linerange(aes(xmin = per_0.25, xmax = per_0.75), alpha = 0.4, size = 3) +
      geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9), fatten = 2, size = 0.4, width = 0.2) +
      geom_linerange(aes(xmin = per_0.05, xmax = per_0.95), size = 0.4) +
      geom_point(aes(x = mean_est), size = 2) +
      geom_point(aes(x = mean_est), color = "white", size = 0.8) +
      scale_x_continuous("", labels = scales::label_number(suffix = " KSh")) +
      # coord_cartesian(xlim = c(-55, 55)) +
      theme(axis.text.y = element_blank()) +
      labs(
        title = "Difference in Valuation of Calendars and Bracelets",
        # subtitle = "For Each County",
        y = "",
        caption = "Line range: 90% credible interval. Outer box: 80% credible interval. Inner box: 50% credible interval.
                   Thick vertical line: median. Point: mean."
      ),
    
    ncol = 1 , rel_heights = c(1, 0.6)
  )
}

plot_beliefs_est <- function(beliefs_results, top_title = NULL, width = 0.3, crossbar_width = 0.2, include = c("both", "1ord")) {
  include <- rlang::arg_match(include)
  pos_dodge <- position_dodge(width = width)

  first_plot <- beliefs_results$prob_knows %>% 
    filter(ord == 1) %>% 
    ggplot(aes(y = assigned_treatment, group = assigned_dist_group)) +
    geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, color = assigned_dist_group), position = pos_dodge, size = 0.4) +
    geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9, color = assigned_dist_group), position = pos_dodge, fatten = 2, size = 0.4, width = crossbar_width) +
    geom_linerange(aes(xmin = per_0.25, xmax = per_0.75, color = assigned_dist_group), position = pos_dodge, alpha = 0.4, size = 2.5) +
    geom_point(aes(x = per_0.5, color = assigned_dist_group), position = pos_dodge, size = 1.8) +
    geom_point(aes(x = per_0.5), position = pos_dodge, color = "white", size = 0.6) +
    scale_color_canva("", labels = str_to_title, palette = canva_palette_vibrant) + 
    scale_y_discrete("", labels = str_to_title) +
    labs(
      title = "First Order Beliefs",
      subtitle = "Proportion",
      x = "") +
    theme(legend.position = "top") +
    NULL
  
  cowplot::plot_grid(
    if (!is_null(top_title)) { 
      cowplot::ggdraw() +
        cowplot::draw_label(top_title, size = 20, fontface = "italic")
    },
    
    cowplot::plot_grid(
      first_plot +
        theme(
          legend.position = "none"
        ) +
        NULL,
      
      beliefs_results$ate_knows %>% 
        filter(ord == 1, assigned_dist_group_left == assigned_dist_group_right) %>% 
        ggplot(aes(y = assigned_treatment_left, group = assigned_dist_group_left)) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, color = assigned_dist_group_left), position = pos_dodge, size = 0.3) +
        geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9, color = assigned_dist_group_left), position = pos_dodge, fatten = 2, size = 0.4, width = crossbar_width) +
        geom_linerange(aes(xmin = per_0.25, xmax = per_0.75, color = assigned_dist_group_left), position = pos_dodge, alpha = 0.4, size = 2.25) +
        geom_point(aes(x = per_0.5, color = assigned_dist_group_left), position = pos_dodge, size = 1.8) +
        geom_point(aes(x = per_0.5), position = pos_dodge, color = "white", size = 0.6) +
        scale_y_discrete(drop = FALSE) +
        scale_color_canva("", labels = str_to_title, palette = canva_palette_vibrant) + 
        labs(
          title = "",
          subtitle = "Treatment Effect",
          x = "", y = "") +
        theme(
          axis.text.y = element_blank(),
          legend.position = "none"
        ) +
        NULL, 
      
      if (include == "both") beliefs_results$prob_knows %>% 
        filter(ord == 2) %>% 
        ggplot(aes(y = assigned_treatment, group = assigned_dist_group)) +
        geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, color = assigned_dist_group), position = pos_dodge, size = 0.4) +
        geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9, color = assigned_dist_group), position = pos_dodge, fatten = 2, size = 0.4, width = crossbar_width) +
        geom_linerange(aes(xmin = per_0.25, xmax = per_0.75, color = assigned_dist_group), position = pos_dodge, alpha = 0.4, size = 2.5) +
        geom_point(aes(x = per_0.5, color = assigned_dist_group), position = pos_dodge, size = 1.8) +
        geom_point(aes(x = per_0.5), position = pos_dodge, color = "white", size = 0.6) +
        scale_color_canva("", labels = str_to_title, palette = canva_palette_vibrant) + 
        scale_y_discrete("", labels = str_to_title) +
        labs(
          title = "Second Order Beliefs",
          subtitle = "Proportion",
          x = "") +
        theme(
          legend.position = "none"
        ) +
        NULL,
      
      if (include == "both") beliefs_results$ate_knows %>% 
        filter(ord == 2, assigned_dist_group_left == assigned_dist_group_right) %>% 
        ggplot(aes(y = assigned_treatment_left, group = assigned_dist_group_left)) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        geom_linerange(aes(xmin = per_0.05, xmax = per_0.95, color = assigned_dist_group_left), position = pos_dodge, size = 0.3) +
        geom_crossbar(aes(x = per_0.5, xmin = per_0.1, xmax = per_0.9, color = assigned_dist_group_left), position = pos_dodge, fatten = 2, size = 0.4, width = crossbar_width) +
        geom_linerange(aes(xmin = per_0.25, xmax = per_0.75, color = assigned_dist_group_left), position = pos_dodge, alpha = 0.4, size = 2.25) +
        geom_point(aes(x = per_0.5, color = assigned_dist_group_left), position = pos_dodge, size = 1.8) +
        geom_point(aes(x = per_0.5), position = pos_dodge, color = "white", size = 0.6) +
        scale_y_discrete(drop = FALSE) +
        scale_color_canva("", labels = str_to_title, palette = canva_palette_vibrant) + 
        labs(
          title = "",
          subtitle = "Treatment Effect",
          x = "", y = "",
          caption = "Line range: 90% credible interval. Outer box: 80% credible interval. Inner box: 50% credible interval. Thick vertical line: median. Point: mean."
        ) +
        theme(
          axis.text.y = element_blank(),
          legend.position = "none",
          plot.caption = element_text(size = 8)
        ) +
        NULL,
      
      ncol = 2, axis = "b", align = "h" 
    ),
    
    cowplot::get_legend(first_plot),
    
    ncol = 1, rel_heights = c(if (!is_null(top_title)) 0.1 else 0, 1, 0.08)
  )
}

# Constants ---------------------------------------------------------------

cost_model_types <- create_stan_enum(c("param_kappa", "param_linear", "param_quadratic", "semiparam", 
                                       "param_linear_salience", "param_quadratic_salience", "semiparam_salience",
                                       "discrete"), "cost_model_type") 
