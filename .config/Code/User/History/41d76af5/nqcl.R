set.seed(24)
library(baggr)
options(mc.cores = 6)
source("data_prep.R")
source("helpers.R")
library(purrr)

bsl_pool <- "partial" #choose partial or none for alternative specification
#partial is preferable, but none could be good for debug

# Let's store the default priors to reuse later in this file
def_priors <- list(
  hypermean = normal(0, 5), 
  hypersd = normal(0, 5),
  # baseline risk of mortality, set SD to reach .25 as upper limit
  control = normal(log(.01), (log(.25) - log(.01))/1.96),
  # set 1SD to 10-fold increase in mortality, that is a lot of variation
  control_sd = normal(0, 2.5)
)



# Main model in our analysis
bg_loo <- readRDS("analysis_output/stan/bg_loo.rds")
bg_main <- bg_loo$full_model



# For now no need to do LOO CV, so I use baggr() not loocv()
# (but it could be used for prior selection too)
# (theoretically at least)

alt_priors2 <- list(
  "Turner et al. priors" = 
    list_modify(def_priors, 
                hypermean = student_t(3.4, 0, 0.52),
                hypersd = lognormal(-2.56/2, 1.74/2)),
  "hypermean: Gaussian, 10%; hyperSD: Turner et al." = 
    list_modify(def_priors, 
                hypermean = normal(log(0.9), (log(.9) - log(.7))/2),
                hypersd = lognormal(-2.56/2, 1.74/2)),
  "hypermean: Student T, 10%; hyperSD: Turner et al." = 
    list_modify(def_priors, 
                hypermean = student_t(1, log(0.9), (log(.9) - log(.7))/2),
                hypersd = lognormal(-2.56/2, 1.74/2))
)

bg_alt_models2 <- lapply(alt_priors2,
                        function(x) 
                          bg_p1 <- baggr(
                            baggr_prep(df_rare), 
                            effect = "logOR", 
                            prior = x,
                            model="logit",
                            pooling_control = bsl_pool,
                            chains = 6, iter = 2000))

saveRDS(bg_alt_models2, "analysis_output/stan/bg_alt_priors_8mar2022.rds")



# Outputs:
bg_alt_models  <- readRDS("analysis_output/stan/bg_alt_priors_5mar2022.rds")
bg_alt_models2 <- readRDS("analysis_output/stan/bg_alt_priors_8mar2022.rds")
bg_alt_models[["diffuse prior"]] <- bg_main

bgc <- do.call(baggr_compare, c(bg_alt_models[c(1,3,5)], bg_alt_models2))
bgc
plot(bgc, transform=exp) + theme(legend.direction = "vertical")
lapply(c(bg_alt_models[c(1,3,5)], bg_alt_models2), 
       effect_draw, transform = exp, s = T) %>% bind_rows(.id="model")

# The difference between Gaussian and Student T priors:
1 - sum(exp(rt(1e06, 1)*(log(.9) - log(.7))/2 + log(0.9)) > .6)/1e06
1 - sum(exp(rnorm(1e06)*(log(.9) - log(.7))/2 + log(0.9)) > .6)/1e06
