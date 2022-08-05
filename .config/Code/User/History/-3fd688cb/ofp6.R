source("code/power-functions.R")
library(furrr)


sim_and_fit =  function(draw, hyper_params){

            res = simulate_data(
                seed = draw, 
                hyper_params = hyper_params
                ) %>%
                    fit_estimators()
                    
            res_tidy = res$tidy_tests %>%
                filter(
                    str_detect(term, "treatment")
                ) %>%
                mutate(signif = p.value < 0.05, TE = hyper_params$TEs[2])
    return(res_tidy)
}

thompson_TE = function(signif_vec, TE_vec, batch = 1){
    N = length(signif_vec)
    df = tibble(
        signif = signif_vec,
        TE = TE_vec
    )
    set.seed(N)
    # Conditional Mean Method
    cond_mean_df = df %>%
        mutate(bin = cut_width(TE, 1/N)) %>%
        group_by(TE_bin = bin) %>%
        summarise(
            n_bin = n(), 
            n_succ = sum(signif), 
            pr_signif = mean(signif),
            TE_mean = mean(TE),
            TE_sd = sd(TE))  %>%
        mutate(
            TE_sd = if_else(n_bin == 1, 0, TE_sd),
            p_val = map2_dbl(n_succ, n_bin, ~suppressWarnings(prop.test(x = .x, n = .y, p = 0.8)$p.value)),
            sample_pr = p_val^2/sum(p_val^2)
        ) 
    cond_mean_s_vec = sample(1:nrow(cond_mean_df), batch, replace = TRUE, prob = cond_mean_df$sample_pr)
    mu_vec = cond_mean_df$TE_mean[cond_mean_s_vec]
    sd_vec = cond_mean_df$TE_sd[cond_mean_s_vec]
    cond_mean_next_TEs = rnorm(batch, mu_vec, sd_vec + 1/N)
    # GLM Method
    glm_fit = glm(
        signif_vec ~ TE_vec, 
        family = binomial(link = "logit")
    )
    df =  df %>%
    mutate(
        pr = predict(glm_fit, type = "response"),
        dist = abs(pr - 0.8), 
            rank = rank(dist), 
            close = rank <= 0.25*N) 
    N_ok = df %>%
        filter(close == TRUE) %>%
        nrow() 
    # if (N_ok == 0) {
    #     browser()
    # }

    linear_approx = df %>%
        filter(close == TRUE) %>%
        lm(TE ~ pr, data = .)    
    lin_pred = predict(linear_approx, newdata = list(pr = 0.8), se.fit = TRUE)
    glm_next_TEs = rnorm(batch, mean = lin_pred$fit, sd = lin_pred$se.fit + 1/N)

    next_TEs = c(cond_mean_next_TEs, glm_next_TEs)
    next_TEs = pmax(pmin(next_TEs, 0.85), 0.03)
    if (any(is.na(next_TEs) | is.nan(next_TEs))) {
        browser()
        # stop("NA TEs suggested")
    }
    return(next_TEs)
}







sim_and_fits = function(hyper_params){
    res = map2_dfr(hyper_params, 1:length(hyper_params), ~sim_and_fit(.y, .x))
    return(res)
}




run_sim = function(N, hyper_param_func){
    initial_TEs = seq(from = 0.0, to = 0.2, length.out = 20)

    hps = map(initial_TEs, ~hyper_param_func(TEs = c(0, .x))) 

    res_df = sim_and_fits(hps)
    for (i in 1:N){
        gp_res_df = res_df %>%
            group_by(model, vcv) %>%
            group_split()
        next_TEs = map(
            gp_res_df,
            ~thompson_TE(
                .x$signif,
                .x$TE
            )
        ) %>% unlist()
        hps = map(next_TEs, ~hyper_param_func(TEs = c(0, .x))) 
        new_res = sim_and_fits(hps)
        res_df = bind_rows(
            res_df,
            new_res
        )
    }
    return(res_df)
}

anon_f = function(TEs){
    default_hyper_params(
        dgp = "icc",
        N = 400,
        n_clusters = 400/8,
        baseline_mean  = 0.10,
        rho = 0.1,
        TEs = TEs
    )
}


param_grid = expand.grid(
    N = c(400, 600, 800, 1000, 1200, 1400),
    rho = c(0.1, 0.25, 0.5, 0.75, 0.9, 1.0)
) %>% as_tibble() %>%
    mutate(n_clusters = floor(N/8))


plan(multisession, workers = 6)
sim_df = future_pmap_dfr(
    list(
        param_grid$N,
        param_grid$rho,
        param_grid$n_clusters
    ),
    ~run_sim(
        N = 6,
        hyper_param_func = function(TEs) default_hyper_params(
            TEs = TEs,
            N = ..1,
            rho = ..2,
            n_clusters = ..3,
            dgp = "icc",
            baseline_mean = 0.1
        )
    ),
    .options = furrr_options(
        seed = TRUE
    ),
    .progress = TRUE
)




tictoc::tic()
initial_sim = run_sim(
    N = 10,
    hyper_param_func = function(TEs) default_hyper_params(
        TEs = TEs, 
        dgp = "icc",
        rho = 0.1,
        baseline_mean = 0.1,
        N = 400, n_clusters = 400/8)
) 
tictoc::toc()
initial_sim

initial_sim %>%
    mutate(x = 1:n()) %>%
    filter( x > 25) %>%
    summarise(mean(signif))

initial_sim %>%
    mutate(x = 1:n()) %>%
    filter(vcv == "standard") %>%
    ggplot(aes(
        x = x, 
        y = TE,
        colour = vcv
    )) +
    geom_point() +
    theme_minimal() +
    labs( 
        x = "Iteration", 
        title = "MDE Bandit Kind Of"
    ) +
    geom_smooth()   


initial_sim %>%
    mutate(signif = as.numeric(signif)) %>%
    filter(
        model == "chisq" & vcv == "standard"
    ) %>%
    mutate(bin = cut_width(TE, 0.01)) %>%
    group_by( 
        bin
    ) %>%
    summarise(across(where(is.numeric), mean)) %>%
    ggplot(aes(x = TE, y = signif)) +
    geom_point() + 
    geom_line() +
    geom_hline(yintercept = 0.8)

initial_sim %>%
    group_by(model, vcv) %>%
    mutate(bin = cut_width(TE, 0.005)) %>%
    group_by(TE_bin = bin, model, vcv) %>%
    summarise(pr_signif = mean(signif), 
              n_draws = n())  %>%
    filter( 
        pr_signif > 0.8 & pr_signif < 0.9
    )

ed %>%
    mutate(bin = cut_width(TE, 0.01)) %>%
    group_by(TE_bin = bin) %>%
    summarise(pr_signif = mean(signif), 
              n_draws = n()) 

ed %>%
    mutate(bin = cut_number(TE, nrow(.)/30)) %>%
    group_by(bin) %>%
    summarise(pr_signif = mean(signif), 
              bin_mean = mean(TE))  %>%
    ungroup() %>%
    ggplot(aes( 
        x = bin_mean, 
        y = pr_signif
    )) +
    geom_point() 






ed %>%
  ggplot(aes(
      x = TE, 
      y = as.numeric(signif)
  )) +
  stat_summary_bin(fun.y='mean', bins = 50,
                   color='orange', size=2, geom='point') +
                   geom_hline(yintercept = 0.8)
