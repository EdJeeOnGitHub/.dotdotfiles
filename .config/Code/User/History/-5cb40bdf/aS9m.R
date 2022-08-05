library(tidyverse) # data manipulation
library(tidybayes) # data manipulation... but Bayesian
library(data.table) # data manipulation... but fast
library(furrr) # like purrr but in parallel thanks to future
library(cmdstanr) # stan


################################################################################
##SBC Functions woooooooooooooo#################################################
################################################################################


#' Generate "Data" for simulations
#' 
#' Here data really means hyperparameters such as number of observations, 
#' number of covariates, and who is censored (any data that's not 'directly' 
#' modelled) 
#' 
#' #TODO: Create a meta gen_data function to vary hyper params 
meta_gen_data_function = function(N = 1000,
                                  nc = 3,
                                  nsc = 3,
                                  mu_beta = rep(0, nc),
                                  sigma_beta = rep(1, nc), 
                                  J = 11){
    function(seed) {
        set.seed(seed + 1e6)
        N = N
        nc = nc
        nsc = nsc 
        # TODO: Note hard coded rn
        mu_beta = rnorm(nc, 0, 1)
        sigma_beta = sigma_beta
        site = sample(1:J, size = N, replace = TRUE)

        censoring = rbernoulli(N, p = 0.5)

        return(list(
            N = N,
            nc = nc,
            nsc = nsc,
            mu_beta = mu_beta,
            sigma_beta = sigma_beta,
            censoring = censoring, 
            J = J,
            site = site
        ))
    }
}

#' Generate parameters 
#' 
#' These are the parameters we want to estimate and correspond to the models
#' `parameters` block
meta_gen_params = function(dgp = "rweibull"){
    function(seed,
             data){

        set.seed(seed + 2e6)
        beta = matrix(rnorm(data$J*data$nc, data$mu_beta, data$sigma_beta),
                      nrow = data$J, 
                      ncol = data$nc, 
                      byrow = TRUE)
        

        lshape = rnorm(1, 3, 1)
        shape = exp(lshape)
        if (dgp == "rexp") {
            shape = 1
        } 
        beta_transformed = -1*beta/shape
        return(list(
            beta = beta,
            shape = shape,
            lshape = lshape,
            beta_transformed = beta_transformed
        ))
    
    }

}

#' Generate Modelled Data
#'
#'
#' Using hyper parameters from `gen_data` and parameters from `gen_params`,
#' create our modelled data. That is, create left and interval censored 
#' survival data according to the generative model. 
meta_gen_modeled_data_function = function(dgp = "rweibull"){
    if (dgp == "rweibull") {
        dgp_f = rweibull
    } 
    if (dgp == "rexp") {
        dgp_f = function(x, y, z){rexp(n = x, rate = z)} # y is shape === 1
    }
    function(seed,
             data,
             params) {
        set.seed(seed + 3e6)
        # X indep normals 0, 1 
        X = matrix(
            abs(rnorm(data$N*data$nc)),
            nrow = data$N,
            ncol = data$nc
        )
        X[, 1] = 1
        scale = vector("numeric", length = data$N)
        for (i in 1:data$N) {
            scale[i] = exp(X[i, ] %*% params$beta_transformed[data$site[i], ])
        }
        censoring = data$censoring
        N = data$N
        # Preallocate these to fill, should really just use apply or something
        #  but this matches stan file which makes debugging easier
        interval_left = matrix(data = NA, 
                            nrow = N,
                            ncol = 1)
        interval_right = matrix(data = NA, 
                                nrow = N,
                                ncol = 1)
    for (i in 1:N) {
        if (censoring[i] == 1) {
            interval_left[i, 1] = dgp_f(1, params$shape, scale[i]) 
                # maybe should set this to -Inf to ensure likelihood blows up if 
                # we've incorrectly indexed censoring[i]
            interval_right[i, 1] = Inf 
        } 
        else {
            #! Not sure if this is correct
            # Just draw from two weibulls and take smallest as left censor 
            # largest as right censor
            interval = sort(dgp_f(2, params$shape, scale[i]))
            interval_left[i, 1] = interval[1]
            interval_right[i, 1] = interval[2]
        } 
    } 



    return(list(
        interval_left = interval_left[, 1],
        interval_right = interval_right[, 1],
        X = X
    ))
    }

}
# gen_modeled_data <- function(seed,
#                              data,
#                              params) {
#   set.seed(seed + 3e6)
    

#     # X indep normals 0, 1 
#     X = matrix(
#         rnorm(data$N*data$nc),
#         nrow = data$N,
#         ncol = data$nc
#     )
    
#     scale = exp(X %*% params$beta_transformed)
#     censoring = data$censoring
#     N = data$N
#     # Preallocate these to fill, should really just use apply or something
#     #  but this matches stan file which makes debugging easier
#     interval_left = matrix(data = NA, 
#                            nrow = N,
#                            ncol = 1)
#     interval_right = matrix(data = NA, 
#                             nrow = N,
#                             ncol = 1)
#    for (i in 1:N) {
#        if (censoring[i] == 1) {
#            interval_left[i, 1] = rweibull(1, params$shape, scale[i]) 
#             # maybe should set this to -Inf to ensure likelihood blows up if 
#             # we've incorrectly indexed censoring[i]
#            interval_right[i, 1] = Inf 
#        } 
#        else {
#            #! Not sure if this is correct
#            # Just draw from two weibulls and take smallest as left censor 
#            # largest as right censor
#            interval = sort(rweibull(2, params$shape, scale[i]))
#            interval_left[i, 1] = interval[1]
#            interval_right[i, 1] = interval[2]
#        } 
#    } 



#   return(list(
#       interval_left = interval_left[, 1],
#       interval_right = interval_right[, 1],
#       X = X
#   ))
# }


#' Sampling wrapper function
#' 
#' N.B. cmdstanr syntax `modelobject$sample()`
sample_from_model <- function(seed,
                              data,
                              params,
                              modeled_data,
                              iters, 
                              stan_model) {
    data_for_stan <- c(data, modeled_data)
    fit = stan_model$sample(
                  data = data_for_stan,
                  seed = seed,
                  chains = 1,
                  init = NULL,
                  refresh = 0, 
                  show_messages = FALSE
    )
    return(fit)
}


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
#' Create a 'comparison df'
#' 
#' We want to compare our \theta_0 to \hat{\theta} draws and check if model 
#' can recover it's own parameters.
#' 
#' Therefore, we extract model draws and tidy them up. Then we extract 
#' generative prior draw and tidy up. Finally, we left_join
create_comp_df = function(model_fit, params){
    find_me = paste0(names(params), collapse = "|")
    long_draw_df = tidy_draws(model_fit) %>%
        pivot_longer(cols = matches(find_me)) %>%
        select(-contains("__")) %>%
        separate(name, into = c("term", "j", "k"), sep = "\\[|,", extra = "drop") %>%
        mutate(
            across(.cols = c(j, k ), ~str_remove(.x, "]") %>% as.numeric)
        ) %>%
        mutate(
            across(.cols = c(j, k ), ~replace_na(.x, 1))
        )

    long_param_df = 1:length(params) %>%
        map_dfr(~flatten_params(params[[.x]]) %>% mutate(term = names(params)[.x])) %>%
        as_tibble()
    comp_df = left_join(
        long_draw_df, 
        long_param_df,
        by = c("term", "j", "k")
    )


    if (nrow(comp_df) != nrow(long_draw_df)) {
        stop("`create_comp_df()` left_join failed.")
    }
    return(comp_df)

}


#' Create Rank Statistic DF
#' 
#' Calculate how often \hat{\theta} < \theta_0 across posterior draws
#' 
#' N.B. This is hardcoded to index \beta[j] at the moment and will have to 
#' be adjusted if we add e.g. \beta[j, t] or some multiple index covariance 
#' matrix
create_rank_stat = function(comp_df) {
    
    rank_df = comp_df %>%
        group_by(term, j, k) %>%
        summarise(rank_stat = mean(value < prior_draw))
    return(rank_df)

}

#' Extract N Effective Draws from cmdstanr fitted object
#' 
#'  Creates a table with each parameters number of effective draws.
#' 
#' I am not a smart man - this is not a good implementation.
#' 
#' For whatever reason, the R side summary functions don't seem to report N_eff
#' but the cmdstan side does. However, N_eff is only passed to us through 
#' `stdout` so I wrote this horrific code to extract the string as a table 
#' and then manipulate this string table into something resembling data. 
#' 
#' An alternative would be to calculate N_eff directly (1 /sum(\hat{\rho}_j)) 
#' or whatever or figure out where on earth this is reported (it must be 
#' somewhere?).
#' #TODO: Fix this bigly edward.
extract_N_eff_badly = function(model_fit){
    # This always prints to stdout even with `invisible` :(
    mod_summary = model_fit$cmdstan_summary()

    # convert to string table :) ... :(
    string_table = read.table(
        textConnection(mod_summary$stdout),
        sep = "\t",
        skip = 5
    ) %>% as_tibble()
    clean_N_eff = string_table %>% 
        # let's hope there's only ever 9 useless params reported at the fron
        slice(9:(nrow(string_table) - 4)) %>% 
        # split if we see a space followed by a digit, word, +, or -
        separate(V1, sep = "\\s(?=(\\w+|\\d+|[\\+\\-]))", into = paste0("col_", 1:10), extra = "drop") %>%
        select(col_1, col_8) %>%
        # clean up the columnns we've grabbed
        mutate(N_eff = as.numeric(col_8), term = str_remove_all(col_1, " ")) %>%
        select(-col_8, -col_1) %>%
        separate(term, into = c("term", "j", "k"), sep = "\\[|,", extra = "drop") %>%
        mutate(
            across(.cols = c(j, k ), ~str_remove(.x, "]") %>% as.numeric)
        )
    return(clean_N_eff)
}
#' Simulation Based Calibration Draw
#' 
#' Takes in a seed and spits out one simulated draw.
#' 
#'  SBC samples from prior, uses model to create data, fits model to data 
#' and compares posterior to sampled prior.
#' 
#' Option to `thin` or not because of the horrific N_eff extraction.
#' If something is breaking turning thin off will be a good idea.
#' 
#'  THINNING
#' Why are we thinning? Because markov chains tend to be auto correlated and 
#' so we want to thin so we're comparing independent quantiles so our rank 
#' stat is well behaved.
#' 
#' We do that by essentially taking N/N_eff every other draw.
#' 
#' Unfortunately NUTS is so good we actually get N_eff > N often because 
#' we find a great spot (thanks HMC) and then NUTS (No U-Turn Sampler)
#'  swings us in the other direction and we end up with negatively 
#' autocorrelated draws => N_eff > N.
#' 
#' Gelman and co/others keep running till N_eff < N and thin every other draw 
#' anyway but I have opted to skip this step. Possible #TODO
sbc_draw = function(seed = 1234,
                    thin = TRUE, 
                    dgp, 
                    stan_model,
                    hyper_params_data){ 

    # TODO remove this and just make named args in sbc_draw()
    gen_data = meta_gen_data_function(
        N = hyper_params_data$N,
        nc = hyper_params_data$nc,
        nsc = hyper_params_data$nsc,
        mu_beta = hyper_params_data$mu_beta,
        sigma_beta = hyper_params_data$sigma_beta,
        J = hyper_params_data$J
    )
    gen_modeled_data = meta_gen_modeled_data_function(dgp = dgp)
    gen_params = meta_gen_params(dgp = dgp)
    data = gen_data(seed)
    params = gen_params(seed,
                        data)
    modeled_data = gen_modeled_data(seed,
                                    data,
                                    params)
    fit = sample_from_model(seed,
                            data,
                            params,
                            modeled_data, 
                            stan_model = stan_model)
    comp_df = create_comp_df(fit, params)
    


    # thinning code
    if (thin == TRUE) {
        # Only extract parameters we care about, if we want to ignore a parameter
        # give it an underscore name or call it TRANSformed_variable.
        N_eff_df = extract_N_eff_badly(
            fit
            )  %>%
                mutate(
                    across(
                        .cols = c("j", "k"),
                        ~replace_na(.x, 1)
                    )
                )

        comp_df = comp_df %>%
            left_join(
                N_eff_df %>%
                    mutate(N = fit$metadata()$iter_sampling, 
                        thin = ceiling(N/N_eff)),
                by = c("term", "j", "k")
            ) %>%
            mutate(thin_draw = .iteration %% thin )
    } else {
        comp_df$thin_draw = 0
    }
    rank_stat = comp_df %>%
        filter(thin_draw == 0) %>%
        create_rank_stat()


  return(rank_stat)
}


# surv_model = cmdstan_model("stan/simple-weibull.stan")
# #####
# seed = 2
# seed = 1


# seed = seed + 1
# print(seed)
# N = 1000
anon_f = function(seed) {

    gen_data = meta_gen_data_function()
    gen_modeled_data = meta_gen_modeled_data_function()
    gen_params = meta_gen_params()
    data = gen_data(seed)
    params = gen_params(seed,
                        data)
    modeled_data = gen_modeled_data(seed,
                                    data,
                                    params)
    return(list(params = params, modeled_data = modeled_data))
}



# draws = map(1:50, anon_f)
# data = map(draws, "modeled_data") %>%
#     map(as_tibble)

# data = imap_dfr(data, ~mutate(.x, draw = .y))
# data %>%
#     ggplot(aes(x = interval_right, fill = factor(draw))) +
#     geom_histogram() 



# params = map(draws, "params") %>%
#     imap_dfr(~as_tibble(.x) %>% mutate(draw = .y))


# params
# data %>% filter(is.finite(interval_right)) %>% filter(interval_right > 10)
# data %>%
#     filter( > interval_right)


# # # modeled_data = c(modeled_data, list(J = J, site = sample(1:J, N, replace = TRUE)))
# data_for_stan <- c(data, modeled_data)

# as_tibble(modeled_data) %>%
#     mutate(cens = data$censoring) %>%
#     filter(cens == FALSE) %>%
#     gather(variable, value, interval_left, interval_right) %>%
#     ggplot(aes(x = value)) +
#     geom_histogram() +
#     facet_wrap(~variable)
# colMeans(params$beta)
# colMeans(params$beta_transformed)
# params$shape
# modeled_data$interval_left %>% max()
# scale = exp(modeled_data$X %*% params$beta_transformed[1, ])
# hist(scale)

# hier_surv_model = cmdstan_model("stan/hierarchical-weibull.stan")

# options(mc.cores = 4)
# stan_fit = hier_surv_model$sample(
#     data_for_stan, 
#     chains = 4,
#     init = NULL,
#     adapt_delta = 0.95
# )

# hist(rweibull(100, shape = 3.2, scale = 91))
# params


# shinystan::launch_shinystan(rstan::read_stan_csv(stan_fit$output_files()))


#####
hier_model = cmdstan_model("stan/hierarchical-weibull.stan")

#####

data_params = list(
    N = 1000,
    nc = 3,
    nsc = 3,
    mu_beta = rep(0, 3),
    sigma_beta = rep(1, 3), 
    J = 5)
# INIT_GLOBAL = NULL
sbc_draw(14, dgp = "rweibull", stan_model = hier_model, hyper_params_data = data_params)


# problem = anon_f(14)
# exp(rlnorm(1000)) %>%  hist()

# tibble(x = exp(log(abs(rnorm(1000))))) %>%
#     ggplot(aes(x = x)) +
#     geom_histogram()




# problem$params

# as_tibble(problem$modeled_data) %>%
#     ggplot(aes(x = interval_left)) +
#     geom_histogram()


# prob_dat = as_tibble(problem$modeled_data)

# prob_dat %>% 
#     filter(interval_right < interval_left)
# prob_dat %>%
#     select(interval_right) %>%
#     filter(is.finite(interval_right)) %>%
#     summarise(max(interval_right))
poss_sbc = possibly(sbc_draw, otherwise = tibble(fail = TRUE))
plan(multicore, workers = 8)
hier_draws <- 1:500 %>%
  future_map_dfr(
          ~poss_sbc(.x, 
                   dgp = "rweibull", 
                   stan_model = hier_model, 
                   hyper_params_data = data_params) %>% 
            mutate(draw = .x),
                 .options = furrr_options(
                     seed = TRUE,
                     packages = c("data.table"),
                     scheduling = 2L # This means we move jobs dynamically
                 ),
                 .progress = TRUE)  

hier_draws %>%
    filter(term == "beta") %>% 
    filter(k == 2) %>%
    ggplot(aes(x = rank_stat)) +
    geom_histogram(bins = 60) +
    facet_wrap(~j ) +
    labs( 
        title = "SBC rank histograms - Hierarchical Weibull - beta_2 J1-J5 "
    )


ggsave("data/plots/hier-weibull-beta-hist.png", width = 8, height = 6)

hier_draws %>%
    select(term) %>%
    unique()
hier_draws %>%
    filter(term == "shape") %>% 
    filter(k == 1) %>%
    ggplot(aes(x = rank_stat)) +
    geom_histogram() +
    facet_wrap(~j )

hier_draws %>%
    filter(term == "shape") %>% 
    ggplot(aes(sample = rank_stat,
           colour = term)) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(term~j,
             scales = "free") +
  theme_bw() +
  guides(colour = "none")  +
  labs(
      title = "SBC - Hierarchical Weibull - Shape parameter", 
      x = "Theoretical Quantile", 
      y = "Realised Quantile"
  )

# ggsave("data/plots/hier-weibull-shape.png", width = 8, height = 6)


hier_draws %>%
    filter(term == "beta") %>% 
    filter(k != 1) %>%
    ggplot(aes(sample = rank_stat,
           colour = factor(k))) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_grid(j~k,
             scales = "free") +
  theme_bw() +
  guides(colour = "none") +
  labs( 
      title = "SBC - Hierarchical Weibull - beta_2 and beta_3 across sites J_1-J_5",
      x = "Theoretical Quantile", 
      y = "Realised Quantile"
  )
# ggsave("data/plots/hier-weibull-beta.png", width = 12, height = 8)
################################################################################
###Simulation Time!!!!!!!!!!!###################################################
################################################################################


# Load our vanilla survival model
exp_model = cmdstan_model("stan/simple-exponential.stan")
wei_model = cmdstan_model("stan/simple-weibull.stan")
plan(multicore, workers = 8)

exp_draws_exp_model <- 1:200 %>%
  future_map_dfr(~sbc_draw(.x, dgp = "rexp", stan_model = exp_model) %>% mutate(draw = .x),
                 .options = furrr_options(
                     seed = TRUE,
                     scheduling = 2L # This means we move jobs dynamically
                 ),
                 .progress = TRUE)  

wei_draws_wei_model <- 1:200 %>%
  future_map_dfr(~sbc_draw(.x, dgp = "rweibull", stan_model = wei_model) %>% mutate(draw = .x),
                 .options = furrr_options(
                     seed = TRUE,
                     scheduling = 2L # This means we move jobs dynamically
                 ),
                 .progress = TRUE)  


exp_draws_wei_model <- 1:500 %>%
  future_map_dfr(~sbc_draw(.x, dgp = "rexp", stan_model = wei_model) %>% mutate(draw = .x),
                 .options = furrr_options(
                     seed = TRUE,
                     scheduling = 2L # This means we move jobs dynamically
                 ),
                 .progress = TRUE)  


plot_sbc = function(sbc_df){
    p = sbc_df %>%
    ggplot(aes(sample = rank_stat,
           colour = term)) +
  stat_qq(distribution = stats::qunif,
          alpha = 1) +
  stat_qq_line(distribution = stats::qunif,
               colour = "black",linetype = "longdash") +
  facet_wrap(term~j ,
             scales = "free") +
  theme_bw() +
  guides(colour = "none") 
  return(p)
}
exp_draws_exp_model %>% 
plot_sbc() +
  labs(
      title = "Exponential Survival Model Simulation Based Calibration",
      subtitle = "Left and Interval Censoring", 
      caption = "Uniform CDF indicates well calibrated posterior coverage.",
      x = "Theoretical Quantile", 
      y = "Realised Quantile"
  )

ggsave("data/plots/exp-exp.png", width = 8, height = 6)

wei_draws_wei_model %>% 
plot_sbc() +
  labs(
      title = "Weibull Survival Model Simulation Based Calibration",
      subtitle = "Left and Interval Censoring", 
      caption = "Uniform CDF indicates well calibrated posterior coverage.",
      x = "Theoretical Quantile", 
      y = "Realised Quantile"
  )

ggsave("data/plots/weibull-sbc.png", width = 8, height = 6)
exp_draws_wei_model %>% 
plot_sbc() +
  labs(
      title = "Weibull Survival Model, Exponential DGP Simulation Based Calibration",
      subtitle = "Left and Interval Censoring", 
      caption = "Uniform CDF indicates well calibrated posterior coverage.",
      x = "Theoretical Quantile", 
      y = "Realised Quantile"
  )

ggsave("data/plots/misspec-weibull-sbc.png", width = 8, height = 6)
exp_draws_wei_model  %>%
    ggplot(aes( 
        x = rank_stat, 
        fill = term,
    )) +
    geom_histogram(bins = 60) +
    facet_wrap(term ~ j, scales = "free") +
    theme_bw() +
    labs(title = "Weibull Model and Exponential DGP SBC", 
        subtitle = "Histogram of rank statistics")
ggsave(
    "data/plots/mis-specified-weibull.png",
    width = 8,
    height = 6
)


################ Scratchpad Stuff ##############################################
# Delete for future use #
######## Tests ##########  
# seed = 1
# seed = seed + 1
# sbc_draw(seed)
# # print(seed)
# # # seed = 1
# data <- gen_data(seed)
# params <- gen_params(seed,
#                     data)
# modelled_data <- gen_modeled_data(seed,
#                                 data,
#                                 params)
# dt = data.table(
#     il = modelled_data$interval_left,
#     ir = modelled_data$interval_right,
#     censored = data$censoring
# )
# dt[, c("x_1", "x_2") :=  as.data.table(modelled_data$X)]

# dt
# dt[censored == FALSE] %>%
#     ggplot(aes( 
#         x = ir
#     )) +
#     geom_histogram()
# params
# rweibull(100, params$shape, 100)
# data_for_stan <- c(data, modelled_data)

#     fit = surv_model$sample(
#                   data = data_for_stan,
#                   seed = seed,
#                   chains = 1
#     )



# ed = extract_N_eff_badly(fit)
# ed = fit$cmdstan_summary()
# tab = read.table(textConnection(ed$stdout), sep = "\t", skip = 5) %>% as_tibble()
# tab
# tab %>%
# tab


# # assuming 'fit' is from CmdStanR
# stanfit <- rstan::read_stan_csv(fit$output_files())
# launch_shinystan(stanfit)
# ed %>%
#     gather_draws(beta[j], shape[j])
# comp_df %>%
#     group_by(term, j) %>%
#     summarise(rank_stat = mean(estimate < prior_draw))

# comp_df = create_rank_df(ed, params)


# param_df 
# param_df

# param_df = map(params, ~t(tibble(paste0(names(.x), ))))


# tibble(c(1, 2, 3), t(params$beta))
# enframe(params$beta)
