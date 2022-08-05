

meta_gen_data = function(gen_data_args){
  function(seed) {
    set.seed(seed + 129)
    N = gen_data_args$N 
    cutoff_quantile = gen_data_args$cutoff_quantile
    p = gen_data_args$p
    q = gen_data_args$q
    d = gen_data_args$d
    return(list(
      N = N,
      cutoff_quantile = cutoff_quantile,
      p = p,
      q = q,
      d = d
    ))
  }
}

meta_gen_params = function(gen_params_args) {
  function(seed, data){
    d = data$d
    set.seed(1212 + seed)
    beta = rnorm(
      n = data$p,
      mean = gen_params_args$mu_beta, 
      sd = gen_params_args$sd_beta)
    if (d == 1) {
      gamma = rnorm(
        n = data$q,
        mean = gen_params_args$mu_beta, 
        sd = gen_params_args$sd_beta)
    } else {
      gamma = MASS::mvrnorm(n = data$p, mu = rep(0, d), Sigma = diag(d) )
    } 
    sigma_y = rgamma(1, shape = 2, scale = 2)
    sigma_h = rgamma(1, shape = 2, scale = 2)
    # rho = runif(1, -1, 1)
    # rho = runif(1, 0.5, 0.95)
    rho =  rbeta(1, 3, 3)
    rho = ((-1)^rbernoulli(1, 0.5))*rho


    Sigma = matrix(c(
      sigma_y, rho*sqrt(sigma_y)*sqrt(sigma_h),
      rho*sqrt(sigma_y)*sqrt(sigma_h), sigma_h
    ), ncol = 2, nrow = 2)
    return(
      list(
        beta = beta,
        gamma = gamma,
        Sigma = Sigma, 
        rho = rho
      )
    )
  }
}

meta_gen_modelled_data = function(gen_modelled_data_args){
    selection_type = gen_modelled_data_args$selection_type
    selection_d = gen_modelled_data_args$selection_d

    sel_args = c("conventional", "none", "quantile")
    if (!(selection_type %in% sel_args)) {
        stop(paste0("selection_type must be one of: ", paste(sel_args, collapse = ", ")))
    }

    rubin = gen_modelled_data_args$rubin

    function(seed, data, params){
    set.seed(seed + 2323)

    
    beta = params$beta
    gamma = params$gamma
    Sigma = params$Sigma
    


      e = MASS::mvrnorm(
          n = data$N,
          mu = rep(0, 2),
          Sigma = Sigma
      )
    if (data$d > 1) {
      es = replicate(data$d-1,
      MASS::mvrnorm(n = data$N, mu = rep(0, 2), Sigma = Sigma) )
      h_errors = es[, 2, ] 
      e = cbind(e, h_errors)
    }
    X = matrix(
        rnorm(data$N*data$p),
        nrow = data$N,
        ncol = data$p
    )
    Z = matrix(
        rnorm(data$N*data$q),
        nrow = data$N,
        ncol = data$q
    )
    X[, 1] = 1
    # Z[, 1] = 1
    Z = X
    h = Z %*% gamma + e[, 2:ncol(e)]
    if (selection_type == "conventional") {
        if (selection_d == 1) {
          D = matrix(1L * (h[, 1] > 0), ncol = 1)
        } else {
          D = matrix(1L * (rowSums(h[, 1:selection_d] > 0) > 0), ncol = 1)
        } 
    }
    if (selection_type == "none") {
        D = matrix(rep(1, data$N), ncol = 1)
    }
    if (selection_type == "quantile") {
      if (selection_d == 1) {
        D = matrix(1L * (h < quantile(h, data$cutoff_quantile)), ncol = 1)
      } else {
        h_sel_rs = rowSums(apply(h[, 1:selection_d], 2, function(x){x < quantile(x, data$cutoff_quantile)} ))
        D = matrix(1L * (h_sel_rs > 0), ncol = 1)
      }
    } 
    # Currently Y isn't a function of H or Z
    y = (X %*% beta +  e[, 1])

    if (rubin == TRUE) {
      y_se = rbeta(n = data$N, shape1 = 3, shape2 = 3)
      y_latent = y
      y = y_latent + rnorm(n = data$N, mean = 0, sd = y_se)
    } else {
      y_se = NULL
    }
    

  if (selection_d < data$d) {
    splits = split(1:ncol(h), f = ceiling(seq_along(1:ncol(h))/selection_d))
    h = lapply(splits, function(x){h[, x]})
    N_choices = length(h)

  } else {
    N_choices = 1
  }
    D_ind = tibble(
      D = D[, 1]
    ) %>%
    mutate(n = 1:n()) %>%
    group_by(D) %>%
    mutate(D_ind = 1:n(), 
          D_ind = if_else(D == 0, 0L, D_ind)) %>%
    ungroup() %>%
    select(D_ind) %>%
    pull()
    return(list(
        X = X,
        Z = Z,
        y = y[, 1],
        D = D[, 1],
        h = h,
        d_selection = selection_d,
        N_choices = N_choices,
        N_obs = sum(D[, 1]),
        D_ind = D_ind,
        y_latent_se_hat = y_se[D[, 1] == 1]
    ))
    }
}



sample_from_model = function(seed, 
                             data,
                             params,
                             modelled_data,
                             stan_model) {
    data_for_stan = c(data, modelled_data)
    fit = sampling(
      stan_model,
      data = data_for_stan,
      seed = seed,
      chains = 1
    )
    return(fit)
}
#' Converts NxK matrix into NK x 1 data.table with columns j, k indexing 
#' matrix indices
flatten_params = function(param_obj) {
    n_r = nrow(param_obj)
    n_c = ncol(param_obj)

    if (!is.null(n_r) & !is.null(n_c)) {
        rownames(param_obj) <- 1:n_r
        colnames(param_obj) <- 1:ncol(param_obj)
        k = rep(colnames(param_obj), each = n_r)
        param_dt = data.table::data.table(j  = rownames(param_obj), k = k, prior_draw = c(param_obj))
    }
    if (!is.null(n_r) & is.null(n_c)){
        rownames(param_obj) <- 1:n_r
        param_dt = data.table::data.table(j  = rownames(param_obj), k = 1, prior_draw = c(param_obj))

    } 
    if (is.null(n_r) & !is.null(n_c)){
        colnames(param_obj) <- 1:n_c
        param_dt = data.table::data.table(j  = 1, k = colnames(param_obj), prior_draw = c(param_obj))

    } 
    
    if (is.null(n_r) & is.null(n_c)){
        param_dt = data.table::data.table(j  = 1, k = 1, prior_draw = c(param_obj))

    } 
    param_dt[, j := as.numeric(j)]
    param_dt[, k := as.numeric(k)]
    return(param_dt)
}
create_comp_df = function(model_fit, params){
    find_me = paste0(names(params), collapse = "|")
    
    long_draw_df = 
        tidy_draws(model_fit) %>%
            select(matches(find_me, ignore.case = FALSE), .iteration) %>%
            pivot_longer(cols = matches(find_me, ignore.case = FALSE)) %>%
            separate(name, into = c("term", "j", "k"), sep = "\\[|,", extra = "drop", fill = "right") %>%
            mutate(
                across(.cols = c(j, k ), ~str_remove(.x, "]") %>% as.numeric)
            ) %>%
            mutate(
                across(.cols = c(j, k ), ~replace_na(.x, 1))
            )
    flat_Sigma = params$Sigma %>%
        flatten_params() %>%
        rename(true_values = prior_draw) %>%
        mutate(term = "Sigma") %>%
        mutate(param_names = paste0("Sigma", 1:n()))
    if (is.null(dim(params$gamma))) {
      gamma = matrix(params$gamma, ncol = 1)
    } else {
      gamma = params$gamma
    }

    flat_gamma = gamma %>%
      flatten_params() %>%
      rename(true_values = prior_draw) %>%
      mutate(term = "gamma") %>%
      mutate(param_names = paste0("gamma", 1:n()))
    N_eff = summary(model_fit)$summary[, "n_eff"]
    
    N_eff_df = tibble(
        N_eff = unlist(N_eff),
        param_names = names(unlist(N_eff))
    )
    clean_N_eff_df = N_eff_df %>%
            separate(param_names, into = c("term", "j", "k"), sep = "\\[|,", extra = "drop", fill = "right") %>%
            mutate(
                across(.cols = c(j, k ), ~str_remove(.x, "]") %>% as.numeric)
            ) %>%
            mutate(
                across(.cols = c(j, k ), ~replace_na(.x, 1))
            )
    true_param_df = tibble(
        true_values = unlist(params),
        param_names = names(unlist(params))
    ) %>%
        mutate(j = str_extract(param_names, "\\d+$"), j = replace_na(j, 1) %>% as.numeric()) %>%
        mutate(term = if_else(
            str_detect(param_names, "\\d+"),
            str_extract(param_names, ".*(?=\\d+)"),
            param_names
        )) %>%
        mutate(k = 1) %>%
        filter(term != "Sigma", term != "gamma") %>%
        bind_rows(
            flat_Sigma,
            flat_gamma
        )
    comp_df = left_join(
        long_draw_df, 
        true_param_df,
        by = c("term", "j", "k")
    ) %>%
        left_join( 
            clean_N_eff_df, 
            by = c("term", "j", "k")
        )
    if (nrow(comp_df) != nrow(long_draw_df)) {
        stop("`create_comp_df()` left_join failed.")
    }
    return(comp_df)

}

simulate_draw = function(seed = 1234,
                    stan_model,
                    fit_model = TRUE,
                    fit_heckit = FALSE,
                    gen_data_args,
                    gen_params_args, 
                    gen_modelled_data_args) {
  gen_data = meta_gen_data(gen_data_args = gen_data_args)
  gen_params = meta_gen_params(gen_params_args = gen_params_args)
  gen_modelled_data = meta_gen_modelled_data(gen_modelled_data_args = gen_modelled_data_args)
  data = gen_data(seed)
  params = gen_params(seed, data)
  modelled_data = gen_modelled_data(seed, data, params)
  if (fit_model == TRUE) {
    if (sum(modelled_data$D) < data$p) {
      ols_fit = NULL
      bayes_fit = NULL
      oracle_ols_fit = NULL 
      heckit_bayes_fit = NULL
    } else {
      bayes_fit = sample_from_model(
          seed,
          data,
          params,
          modelled_data,
          stan_model
      )
      if (fit_heckit == TRUE) {
        heckit_bayes_fit = sample_from_model(
            seed,
            data,
            params,
            modelled_data,
            heckit_model
        )
      } else {
        heckit_bayes_fit = NULL
      }

      ols_fit = lm(modelled_data$y[modelled_data$D > 0] ~ 0 +  modelled_data$X[modelled_data$D > 0, ])
      oracle_ols_fit = lm(modelled_data$y ~ 0 + modelled_data$X)
    }
    return(list(bayes_fit = bayes_fit, 
                heckit_bayes_fit = heckit_bayes_fit,
                ols_fit = ols_fit, 
                oracle_ols_fit = oracle_ols_fit,
                params = params, 
                data = c(data, list(b = modelled_data$b))))
  } 
  if (fit_model == FALSE) {
    return(list(
        params = params,
        data = data,
        modelled_data = modelled_data
    ))
  }
}

create_sbc_output = function(params, stan_fit, N) {
  if (!is.null(stan_fit)) {

    comp_df = create_comp_df(stan_fit, params) %>%
        mutate(
            N = N, 
            thin = ceiling(N/N_eff),
            thin_draw = .iteration %% thin
            ) 
    rank_df = comp_df %>%
        filter(thin_draw == 0) %>%
        group_by(term, j, k) %>%
        summarise(rank_stat = mean(value < true_values), .groups = "drop") 
  } else {
    rank_df = NULL
  }
    return(rank_df)
}


create_hierarchical_comp_df = function(bayes_fit, params, return_true_df = FALSE) {

  gamma = params$gamma
  splits = split(1:ncol(gamma), f = ceiling(seq_along(1:ncol(gamma))/2))

  true_gamma_tidy = lapply(splits, function(x){gamma[, x]}) %>%
    map(flatten_params) %>%
    imap_dfr(~mutate(., c = as.numeric(.y))) %>%
    rename(true_value = prior_draw) %>%
    mutate(term = "gamma")

  true_beta_tidy = params$beta %>%
    matrix(., ncol = 1) %>%
    flatten_params() %>%
    rename(true_value = prior_draw) %>%
    mutate(term = "beta")

  single_sigma_tidy = params$Sigma %>%
    flatten_params()
  true_sigma_tidy = map_dfr(1:length(splits), ~mutate(single_sigma_tidy, c = .x)) %>%
    mutate(term = "Sigma") %>%
    rename(true_value = prior_draw)  


  tidy_beta_draws = bayes_fit %>%
    gather_draws(beta[k])
  tidy_gamma_draws = bayes_fit %>%
    gather_draws(gamma[c, j, k])
  tidy_sigma_draws = bayes_fit %>%
    gather_draws(Sigma[c, j, k])

  true_df = bind_rows(
    true_beta_tidy,
    true_gamma_tidy,
    true_sigma_tidy
  ) %>%
    mutate(c = replace_na(c, 1))
  tidy_draw_df = bind_rows(
    tidy_beta_draws,
    tidy_gamma_draws,
    tidy_sigma_draws
  ) %>%
    mutate(c = replace_na(c, 1))

  comp_df = inner_join(
      true_df,
      tidy_draw_df,
      by = c("term" = ".variable", 
            "j", "c", "k")
  ) %>%
    as_tibble()
  if (return_true_df == TRUE) {
    return(list(
      comp_df = comp_df,
      true_df = true_df
    )) 
  } else {
    return(comp_df)
  }
}

tidy_sim_output = function(sim_output, type = "non-hierarchical") {

  true_params = sim_output$params 
  p = length(true_params$beta)
  if (type == "hierarchical") {
    comp_output = create_hierarchical_comp_df(sim_output$bayes_fit, true_params, return_true_df = TRUE)
    tidy_bayes = comp_output$comp_df %>%
      group_by(term, c, j, k) %>%
      summarise(
        estimate = median(.value), 
        conf.high = quantile(.value, 0.975),
        conf.low = quantile(.value, 0.025), 
        true_value = unique(true_value), 
        .groups = "drop"
      ) %>%
      mutate(model = "bayes")
    true_df = comp_output$true_df
  } else {
    if (!is.null(sim_output$bayes_fit)) {
      tidy_bayes = sim_output$bayes_fit %>%
        gather_draws(beta[j], gamma[j, k], rho, sigma_y, sigma_h) %>%
        median_qi() %>%
        to_broom_names() %>%
        mutate(j = replace_na(j, 1), k = replace_na(k, 1))
      if (!is.null(sim_output$heckit_bayes_fit)) {

        tidy_heckit = sim_output$heckit_bayes_fit %>%
          gather_draws(beta[j], gamma[j], rho, sigma_y, sigma_h) %>%
          median_qi() %>%
          to_broom_names() %>%
          mutate(j = replace_na(j, 1), k = 1) %>% 
          mutate(model = "heckit")

      } else {
        tidy_heckit = NULL
      }
  }
  }


    tidy_freq_ols = sim_output$ols_fit %>%
      tidy(conf.int = TRUE) %>%
      mutate(term = "beta") %>%
      mutate(j = 1:p, k = 1)

    tidy_oracle_ols = sim_output$oracle_ols_fit %>%
      tidy(conf.int = TRUE) %>%
      mutate(term = "beta") %>%
      mutate(j = 1:p, k = 1)    

    clean_true_params = list(
      "sigma_h" = true_params$Sigma[2, 2],
      "sigma_y" = true_params$Sigma[1, 1],
      "rho" = true_params$rho
    ) %>%
    as_tibble() %>%
    gather(term, true_value) %>%
    mutate(j = 1, k = 1)
    clean_true_gammas = flatten_params(matrix(true_params$gamma, nrow = sim_output$data$q)) %>%
      rename(true_value = prior_draw) %>%
      mutate(term = "gamma")

    clean_true_betas = flatten_params(matrix(true_params$beta, nrow = sim_output$data$p)) %>%
      rename(true_value = prior_draw) %>%
      mutate(term = "beta")

    true_param_df = bind_rows(
      clean_true_params,
      clean_true_betas,
      clean_true_gammas
    )
    if (type == "hierarchical") {
      comp_ols = bind_rows(
        tidy_freq_ols %>% mutate(model = "freq_ols"),
        tidy_oracle_ols %>% mutate(model = "oracle_ols")
      ) %>%
        select(term, j, k, estimate, conf.low, conf.high, model) %>%
        mutate(c = 1) %>%
        left_join(
          true_df, by = c("term", "j", "k", "c")
        )
        comp_df = bind_rows(
          tidy_bayes,
          comp_ols
        )
    } else {
      comp_df = bind_rows(
        tidy_bayes %>% mutate(model = "bayes"),
        tidy_freq_ols %>% mutate(model = "freq_ols"),
        tidy_oracle_ols %>% mutate(model = "oracle_ols"),
        tidy_heckit     ) %>%
        select(term, j, k, estimate, conf.low, conf.high, model) %>%
        left_join(
          true_param_df, by = c("term", "j", "k")
        )
comp_df
    }
  return(comp_df)
}
