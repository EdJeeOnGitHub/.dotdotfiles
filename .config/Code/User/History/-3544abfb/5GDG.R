################# Functions ####################################################

#' We introduce sim_options and hyper_params options as vectors of arguments to 
#' pass to data simulation and power calculation.
#'  
#' For now the distinction between sim options and hyper param options is murky
#' but in general we should think of hyper params as features of DGP/modelling 
#' and sim options as specifics concerning simulation 
#' 
#' In truth p_adjust_methods should probably be in hyper_params options and not
#' sim_options
sim_and_fit = function(draw, hyper_params, sim_options){

            res = simulate_data(
                seed = draw, 
                hyper_params = hyper_params
                ) %>%
                    fit_estimators()
                    
            res_tidy = res$tidy_tests %>%
                filter(
                    str_detect(term, "treatment")
                ) %>%
                mutate(
                    p_adj = map(
                        p.value, 
                        ~p.adjust(.x,
                                  method = sim_options$p_adjust_method, 
                                  n = sim_options$num_p_adjust))
                ) %>%
                mutate(
                    signif = p_adj < 0.05, TE = hyper_params$TEs[2])
    return(res_tidy)
}


#' Thompson Sampling (ish) to search for MDE
#' 
#' 
#' We want to learn about the MDE at a power of 80%.
#' 
#' But first we have to know what TEs will be significant with prob 0.8
#' 
#' There are two steps we need to solve - 1) Estimate where on the power curve 
#' we have an 80% chance of significance. 2) Draw samples in this region often 
#' to get accurate estimates of the MDE.
#'
#'  Each period we use two methods: conditional mean method and GLM.
#' 
#' Conditional Mean Method:
#' Split the TEs into bins and estimate Pr(signif) within that bin. Calculate the 
#' p-value  against H_0: pr(signif) = 0.8. Choose future bins to sample the TE 
#' from in proportion to it's p value. I.e. don't sample often if we reject null of 
#' H_0 = 0.8 
#' 
#' Ideally this would be a beta-bernoulli bandit or even some sort of spline 
#' but this is a hacky frequentist implementation.
#'
#' GLM Method: 
#' 
#' 
#' Step 1)
#' Estimate a logit of signif ~ TE. Use logit fit to generate predicted probabilities 
#' of significance for each TE observed so far. Those that are closest to 0.8 (in the 
#' top 25% of the observations in terms of distance) we take and fit a linear model 
#' of TE ~ \hat{probability} i.e. the inverse regression to before TE ON Pr.
#' 
#' What is this doing? The logit fit is putting structure on the power curve when 
#' we have few observations so we don't spend too much time searching in stupid 
#' places. Then we take a linear approximation to the power curve in the region the 
#' logit thinks has an 80% chance of being significant.
#' 
#' Step 2)
#' Use our estimated linear approximation to predict which MDE will correspond to 
#' an prob of 0.8 and then draw from a random normal centred on this predicted point.
#'
#'
#' We combine the candidate TEs from the conditional mean model and the GLM 
#' because as the data grows we can abandon the structure of the logit for fear 
#' of model misspecification and just use conditional means in buckets to estimate 
#' the power curve. However, the conditional mean method needs a lot of data to 
#' get started so the logit structure helps us in the early stages of exploration.
#' 
#' Ideally we'd phase out the logit as N grows in a data dependent way but for 
#' now we just use both throughout.
thompson_TE = function(signif_vec, TE_vec, batch = 15){
    # Some data prep
    N = length(signif_vec)
    df = tibble(
        signif = signif_vec,
        TE = TE_vec
    )
    # set seed using N TEs so far - want each new draw to be random
    set.seed(N)
    ### Conditional Mean Method ###
    cond_mean_df = tryCatch(
        {

        cond_mean_df = df %>%
            mutate(bin = cut_number(TE, floor(sqrt(N)))) %>% # bin shrink as N grows
            group_by(TE_bin = bin) %>%
            summarise(
                n_bin = n(), 
                n_succ = sum(signif), 
                pr_signif = mean(signif),
                TE_mean = mean(TE),
                TE_sd = sd(TE))  %>%
            mutate(
                TE_sd = if_else(n_bin == 1, 0, TE_sd), # catch cases with 0 obs
                # suppress warnings just kills chi sq warning with low N
                p_val = map2_dbl(n_succ, n_bin, ~suppressWarnings(prop.test(x = .x, n = .y, p = 0.8)$p.value)),
                sample_pr = p_val/sum(p_val) # uhhhhhh this is gross
            ) 
        },
        error = function(cond){
            cond_mean_df = df %>%
                mutate(bin = cut_width(TE, floor(sqrt(N)))) %>% # bin shrink as N grows
                group_by(TE_bin = bin) %>%
                summarise(
                    n_bin = n(), 
                    n_succ = sum(signif), 
                    pr_signif = mean(signif),
                    TE_mean = mean(TE),
                    TE_sd = sd(TE))  %>%
                mutate(
                    TE_sd = if_else(n_bin == 1, 0, TE_sd), # catch cases with 0 obs
                    # suppress warnings just kills chi sq warning with low N
                    p_val = map2_dbl(n_succ, n_bin, ~suppressWarnings(prop.test(x = .x, n = .y, p = 0.8)$p.value)),
                    sample_pr = p_val/sum(p_val) # uhhhhhh this is gross
                ) 

        }
    )

    # Sample a bin to draw from next in proportion to prob it's 80% signif
    cond_mean_s_vec = sample(1:nrow(cond_mean_df), batch, replace = TRUE, prob = cond_mean_df$sample_pr)
    mu_vec = cond_mean_df$TE_mean[cond_mean_s_vec]
    sd_vec = cond_mean_df$TE_sd[cond_mean_s_vec]
    # hacky 1/N in sd to reflect posterior certainty 
    cond_mean_next_TEs = rnorm(batch, mu_vec, sd_vec + 1/N)

    ### GLM Method ###
    glm_fit = glm(
        signif_vec ~ TE_vec, 
        family = binomial(link = "logit")
    )
    if (glm_fit$converged == TRUE) {

        df =  df %>%
        mutate(
            pr = predict(glm_fit, type = "response"),
            dist = abs(pr - 0.8), 
                rank = rank(dist), 
                close = rank <= 0.25*N) 
        N_ok = df %>%
            filter(close == TRUE) %>%
            nrow() 
        if (N_ok > 0) {
            linear_approx = df %>%
                filter(close == TRUE) %>%
                lm(TE ~ pr, data = .)    
            lin_pred = predict(linear_approx, newdata = list(pr = 0.8), se.fit = TRUE)
            glm_next_TEs = rnorm(batch, mean = lin_pred$fit, sd = lin_pred$se.fit + 1/N)
        } else {
            glm_next_TEs = NULL
        }

    } else { # if fit doesn't converge don't suggest any TEs
        glm_next_TEs = NULL 
    }
    next_TEs = c(cond_mean_next_TEs, glm_next_TEs)
    # N.B this is essentially hardcoded for ICC dgp
    next_TEs = pmax(pmin(next_TEs, 1), 0.0) # make sure suggested TEs in [0,1] 
    # If for whatever reason we have naughty TE candidates kill the algo
    if (any(is.na(next_TEs) | is.nan(next_TEs))) {
        stop("NA TEs suggested")
    }
    return(next_TEs)
}



#' Wrapper function to take list of hyper params
sim_and_fits = function(hyper_params, sim_options){
    res = map2_dfr(hyper_params, 1:length(hyper_params), ~sim_and_fit(.y, .x, sim_options = sim_options))
    return(res)
}




#' Run Thompson MDE Simulation
#' 
#' We batch up TE candidates so we don't fit a logit for every single new 
#' observation but rather in sets of size batch
#' 
#' We take a hyper parameter function as an argument. This is due to bad 
#' design decisions - we put TE params in hyper_params vector and the bandit 
#' process would really like to separate immutable hyper params from TEs to 
#' search over.
#' 
#' 
run_sim = function(N, hyper_param_func, batch = 15, sim_options){
    initial_TEs = seq(from = 0.0, to = 0.2, length.out = 20)

    hps = map(initial_TEs, ~hyper_param_func(TEs = c(0, .x))) 
    res_df = sim_and_fits(hps, sim_options = sim_options)

    for (i in 1:N){
        next_TEs = thompson_TE(res_df$signif, res_df$TE, batch = batch)
        hps = map(next_TEs, ~hyper_param_func(TEs = c(0, .x))) 
        new_res = sim_and_fits(hps, sim_options = sim_options)
        # We hate growing DFs but this is probably not the most important thing 
        # to optimise rn.
        res_df = bind_rows(
            res_df,
            new_res
        )
    }
    # Save hyper params and sim options to the df so we have a record of them
    # TODO: Put these into a separate df so we don't save them N*batch times for 
    # no reason.
    hyper_params = hps[[1]]
    res = c(
        hyper_params[!str_detect(names(hyper_params), "dgp|TEs|pr_arm")],
        "pr_arm" = hyper_params$pr_arm,
        c(sim_options)[!str_detect(names(sim_options), "--")]
    ) 
    res_df = bind_cols(
        res_df,
        as_tibble(res)
    )
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



