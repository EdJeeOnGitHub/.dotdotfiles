library(tidyverse)
library(lubridate)
library(did)
library(fixest)
library(furrr)
library(ggstance)
library(lmtest)
library(sandwich)


RCT_sample = c("Hyderabad",
               "Jacobabad",
               "Kambar",
               "Karachi Central",
               "Karachi East",
               "Karachi West",
               "Sujawal")
cohort_vax_df = read_csv("data/output/clean-preprogramme-metrics.csv")

# Sum over birth years to get toal vaxxed in a given month
agg_vax_df = cohort_vax_df %>%
    group_by(vaccine, district, vax_year_mon) %>%
    summarise( 
        n_vaxxed = sum(n_vaxxed), 
        .groups = "drop"
    )

# create numeric time period variables N.B. months hard coded here.
did_vax_df = agg_vax_df %>%
    mutate( 
        t_period = interval(min(vax_year_mon), vax_year_mon) %/% months(1),
        district_id = as.numeric(factor(district)), 
        ln_n_vaxxed = log1p(n_vaxxed)
    ) 


agg_vax_df %>%
    filter(vaccine %in% c("BCG", "Penta-1")) %>%
    ggplot(aes(
        x = vax_year_mon,
        y = n_vaxxed, 
        colour = district)) +
    geom_point() +
    geom_line() +
    facet_wrap(~vaccine, ncol = 1) +
    theme_bw() 
ggsave("data/output/plots/bcg-penta-1-n-vaxxed-plot.png", width = 8, height = 6)

fit_differential_trend_model = function(data, outcome_vaccine) {
    subset_df = data %>%
        filter(vaccine == outcome_vaccine)

    dummy_matrix = subset_df %>%
        model.matrix( ~ 0 + factor(t_period) + factor(district), .) %>%
        as_tibble()

    clean_dummy_colnames = str_replace_all(colnames(dummy_matrix), "factor|\\(|\\)", "")  %>%
        str_replace_all(., " ", "")


    colnames(dummy_matrix) = clean_dummy_colnames
    cols_to_use = clean_dummy_colnames[!str_detect(clean_dummy_colnames, "t_period0|t_period1")]
    form = as.formula( 
        paste0(
            "n_vaxxed ~  t_period:district + ", 
            paste0(
                cols_to_use, 
                collapse = "+"))
    )

    lm_fit = bind_cols( 
        subset_df,
        dummy_matrix
    ) %>%
        lm(data = ., 
        formula = form)

    restricted_fit = bind_cols( 
        subset_df,
        dummy_matrix
    ) %>%
        lm(data = ., 
        formula = update(form, ~. - t_period:district + t_period))
    anova_res = anova(restricted_fit, lm_fit)
    tidy_lm_fit = tidy(lm_fit, conf.int = TRUE)
    
    tidy_hac_lm_fit = coeftest(lm_fit, vcov = vcovHAC(lm_fit, order.by = subset_df$t_period, cluster = subset_df$district)) %>%
        tidy(conf.int = TRUE) %>%
        filter(str_detect(term, "t_period") & str_detect(term, "district")) 
    tidy_hac_lm_fit$vaccine = outcome_vaccine
    tidy_lm_fit$vaccine = outcome_vaccine
    return(list(
        anova_res = anova_res,
        tidy_lm_fit = tidy_lm_fit,
        tidy_hac_lm_fit = tidy_hac_lm_fit
    ))
}

vaccines = unique(agg_vax_df$vaccine)

differential_trend_models = vaccines %>%
    map(fit_differential_trend_model, 
        data = did_vax_df %>% 
            filter(district %in% RCT_sample) %>%
            filter(!(vaccine %in% c("Measles-1", "Measles-2")) | (vaccine == "Measles-1" & t_period >= 10) | (vaccine == "Measles-2" & t_period >= 15))
            )

add_differential_trend_models =  vaccines %>%
    map(fit_differential_trend_model, 
        data = did_vax_df %>% 
            filter(!(vaccine %in% c("Measles-1", "Measles-2")) | (vaccine == "Measles-1" & t_period >= 10) | (vaccine == "Measles-2" & t_period >= 15))
            )

tidy_add_diff_models = map_dfr(add_differential_trend_models, "tidy_lm_fit")
tidy_add_hac_models = map_dfr(add_differential_trend_models, "tidy_hac_lm_fit")

tidy_diff_models = map_dfr(differential_trend_models, "tidy_lm_fit")
tidy_diff_hac_models = map_dfr(differential_trend_models, "tidy_hac_lm_fit")

tidy_add_hac_models %>%
filter(str_detect(term, "t_period") & str_detect(term, "district")) %>%
    mutate(district = str_extract(term, "(?<=district).*")) %>%
    mutate(in_rct = district %in% RCT_sample) %>%
    ggplot(aes( 
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high, 
        y = district, 
        colour = in_rct
    )) +
    geom_pointrangeh() +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs(title = "District Time Trend Estimates", subtitle = "HAC SEs, additional districts included")

ggsave(
    "data/output/plots/add-district-time-trend-pretrend-estimates-HAC.png", 
    width = 8,
    height = 6
)
tidy_diff_hac_models %>%
filter(str_detect(term, "t_period") & str_detect(term, "district")) %>%
    mutate(district = str_extract(term, "(?<=district).*")) %>%
    ggplot(aes( 
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high, 
        y = district
    )) +
    geom_pointrangeh() +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs(title = "District Time Trend Estimates", subtitle = "HAC SEs")

ggsave(
    "data/output/plots/district-time-trend-pretrend-estimates-HAC.png", 
    width = 8,
    height = 6
)

tidy_diff_models %>%
    filter(str_detect(term, "t_period") & str_detect(term, "district")) %>%
    mutate(district = str_extract(term, "(?<=district).*")) %>%
    ggplot(aes( 
        x = estimate, 
        xmin = conf.low, 
        xmax = conf.high, 
        y = district
    )) +
    geom_pointrangeh() +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs(title = "District Time Trend Estimates", subtitle = "Homo SEs")

ggsave(
    "data/output/plots/district-time-trend-pretrend-estimates-Homo.png", 
    width = 8,
    height = 6
)


anova_add_models = map(add_differential_trend_models, "anova_res") %>%
    map_dfr(tidy)

anova_add_table = anova_add_models %>%
    filter(!is.na(p.value)) %>%
    mutate(vaccine = vaccines) %>%
    mutate( 
        adjusted_p_val = p.adjust(p.value),
        signif = adjusted_p_val < 0.05
    ) 
anova_add_table


anova_diff_models = map(differential_trend_models, "anova_res") %>%
    map_dfr(tidy) 
anova_table = anova_diff_models %>%
    filter(!is.na(p.value)) %>%
    mutate(vaccine = vaccines) %>%
    mutate( 
        adjusted_p_val = p.adjust(p.value),
        signif = adjusted_p_val < 0.05
    ) 
    
    
anova_table 
    
anova_table %>%
    select(
        vaccine,
        res.df,
        statistic, 
        p.value,
        adjusted_p_val,
        signif) %>%
    mutate_if(is.numeric, round, 5) %>%
    xtable::xtable() %>%
    print("data/output/tables/diff-trends-F-stat.tex", type = "latex")






did_vax_df %>%
    filter(vaccine %in% c("Measles-1", "Measles-2")) %>%
    ggplot(aes(x = vax_year_mon, y = n_vaxxed, colour = district)) +
    geom_point() +
    geom_line()  +
    facet_wrap(~vaccine, ncol = 1) +
    theme_bw() +
    labs(title = "Measles Data 'missing'", x = "Time")


ggsave("data/output/plots/measles-data-ts.png", width = 8, height = 6)





ip_func = function(y1, y0, D, covariates, i.weights = NULL, boot = FALSE, 
    boot.type = "weighted", nboot = NULL, inffunc = FALSE) {
    D <- as.vector(D)
    n <- length(D)
    deltaY <- as.vector(y1 - y0)
    int.cov <- as.matrix(rep(1, n))
    if (!is.null(covariates)) {
        if (all(as.matrix(covariates)[, 1] == rep(1, n))) {
            int.cov <- as.matrix(covariates)
        }
        else {
            int.cov <- as.matrix(cbind(1, covariates))
        }
    }
    if (is.null(i.weights)) {
        i.weights <- as.vector(rep(1, n))
    }
    else if (min(i.weights) < 0) 
        stop("i.weights must be non-negative")


    # ##
    # browser()
    # PS <- suppressWarnings(stats::glm(D ~ -1 + int.cov, family = "binomial", 
    #     weights = i.weights))
        
    # Hessian.ps <- stats::vcov(PS) * n
    # Hessian.ps
    # if (PS$converged == FALSE) {
    #     warning(" glm algorithm did not converge")
    # }
    # if (anyNA(PS$coefficients)) {
    #     stop("Propensity score model coefficients have NA components. \n Multicollinearity (or lack of variation) of covariates is a likely reason.")
    # }
    # ps.fit <- as.vector(PS$fitted.values)
    # ps.fit <- pmin(ps.fit, 1 - 1e-16)
    ##
    ps.fit = int.cov[, 2]
    if (any(ps.fit == 0 | ps.fit == 1)){
        stop("Propensity score can't equal 0 or 1.")
    }
    w.treat <- i.weights * D
    w.cont <- i.weights * ps.fit * (1 - D)/(1 - ps.fit)
    att.treat <- w.treat * deltaY
    att.cont <- w.cont * deltaY
    eta.treat <- mean(att.treat)/mean(w.treat)
    eta.cont <- mean(att.cont)/mean(w.cont)
    ipw.att <- eta.treat - eta.cont
    score.ps <- i.weights * (D - ps.fit) * int.cov
    # TODO: Desperately fix this
    Hessian.ps <- matrix(c(1, var(ps.fit), 1, var(ps.fit)), ncol = 2) * n
    asy.lin.rep.ps <- score.ps %*% Hessian.ps
    inf.treat <- (att.treat - w.treat * eta.treat)/mean(w.treat)
    inf.cont.1 <- (att.cont - w.cont * eta.cont)
    M2 <- base::colMeans(w.cont * (deltaY - eta.cont) * int.cov)
    inf.cont.2 <- asy.lin.rep.ps %*% M2
    inf.control <- (inf.cont.1 + inf.cont.2)/mean(w.cont)
    att.inf.func <- inf.treat - inf.control
    if (boot == FALSE) {
        se.att <- stats::sd(att.inf.func)/sqrt(n)
        uci <- ipw.att + 1.96 * se.att
        lci <- ipw.att - 1.96 * se.att
        ipw.boot <- NULL
    }
    if (boot == TRUE) {
        if (is.null(nboot) == TRUE) 
            nboot = 999
        if (boot.type == "multiplier") {
            ipw.boot <- mboot.did(att.inf.func, nboot)
            se.att <- stats::IQR(ipw.boot)/(stats::qnorm(0.75) - 
                stats::qnorm(0.25))
            cv <- stats::quantile(abs(ipw.boot/se.att), probs = 0.95)
            uci <- ipw.att + cv * se.att
            lci <- ipw.att - cv * se.att
        }
        else {
            ipw.boot <- unlist(lapply(1:nboot, wboot.std.ipw.panel, 
                n = n, deltaY = deltaY, D = D, int.cov = int.cov, 
                i.weights = i.weights))
            se.att <- stats::IQR(ipw.boot - ipw.att)/(stats::qnorm(0.75) - 
                stats::qnorm(0.25))
            cv <- stats::quantile(abs((ipw.boot - ipw.att)/se.att), 
                probs = 0.95)
            uci <- ipw.att + cv * se.att
            lci <- ipw.att - cv * se.att
        }
    }
    if (inffunc == FALSE) 
        att.inf.func <- NULL
    call.param <- match.call()
    argu <- mget(names(formals()), sys.frame(sys.nframe()))
    boot.type <- ifelse(argu$boot.type == "multiplier", "multiplier", 
        "weighted")
    boot <- ifelse(argu$boot == TRUE, TRUE, FALSE)
    argu <- list(panel = TRUE, normalized = TRUE, boot = boot, 
        boot.type = boot.type, nboot = nboot, type = "ipw")
    ret <- (list(ATT = ipw.att, se = se.att, uci = uci, lci = lci, 
        boots = ipw.boot, att.inf.func = att.inf.func, call.param = call.param, 
        argu = argu))
    class(ret) <- "drdid"
    return(ret)
}

fit_treatment_permutation = function(data, 
                                     vaccines, 
                                     yname = "n_vaxxed", 
                                     RCT_sample = c(
                                        "Hyderabad",
                                        "Jacobabad",
                                        "Kambar",
                                        "Karachi Central",
                                        "Karachi East",
                                        "Karachi West",
                                        "Sujawal"), 
                                    prop_score = FALSE) {
    subset_data = data %>%
        group_by(district) %>%
        mutate(
            fake_treatment_timing = if_else(
                district %in% RCT_sample,
                sample(t_period, 1),
                0)
        ) %>%
        ungroup()

    if (prop_score == TRUE) {
        did_fits = map(
            vaccines,
            ~att_gt(
                data = subset_data %>% filter(vaccine == .x), 
                yname = yname,
                est_method = ip_func, 
                xformla = ~pr_accept,
                tname = "t_period", 
                idname = "district_id",
                gname = "fake_treatment_timing",
                control_group = "notyettreated",
                bstrap = FALSE
            )
        )
    } 
    return(did_fits)
}




sim_function = function(draw, data, vaccines, yname = "n_vaxxed") {
    did_fits = fit_treatment_permutation(
        data = data, 
        vaccines = vaccines,
        yname = yname
    )
    did_es = map(did_fits, aggte, type = "dynamic")

    tidy_es = map_dfr(1:length(vaccines), ~tidy(did_es[[.x]]) %>% mutate(vaccine = vaccines[[.x]]) ) 
    tidy_es$type = "dynamic"

    did_cal = map(did_fits, aggte, type = "calendar")
    tidy_cal = map_dfr(1:length(vaccines), ~tidy(did_cal[[.x]]) %>% mutate(vaccine = vaccines[[.x]]) ) 
    tidy_cal$type = "calendar"
    tidy_df = bind_rows(
        tidy_es,
        tidy_cal
    )
    tidy_df$draw = draw

    return(tidy_df)
}



plan(multisession, workers = 10)


add_pretrend_sim = future_map_dfr(
    1:100,
    ~sim_function(
        draw = .x,
        vaccines = vaccines,
        data = did_vax_df,
        yname = "n_vaxxed"
    ),
    .options = furrr_options(seed = TRUE, packages = "broom"),
    .progress = TRUE
) %>%
    as_tibble()

pretrend_sim = future_map_dfr(
    1:100,
    ~sim_function(
        draw = .x,
        vaccines = vaccines,
        data = did_vax_df %>% filter(district %in% RCT_sample),
        yname = "n_vaxxed"
    ),
    .options = furrr_options(seed = TRUE, packages = "broom"),
    .progress = TRUE
) %>%
    as_tibble()
measles_vec = c("Measles-1", "Measles-2")

pretrend_sim %>%
    filter(type == "calendar")  %>%
    filter( 
        draw == 1
    )   %>%
    ggplot(aes( 
        x = time, 
        y = estimate, 
        ymin = conf.low, 
        ymax = conf.high
    )) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "longdash")  +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs(title = "Calendar Time Event Study", 
          subtitle = "Treatment Timing Randomly Chosen")

ggsave("data/output/plots/pretrend-calendar-time-es-fake.png", width = 8, height = 6)

pretrend_sim %>%
    filter(type == "dynamic")  %>%
    filter( 
        draw == 4
    )   %>%
    ggplot(aes( 
        x = event.time, 
        y = estimate, 
        ymin = conf.low, 
        ymax = conf.high
    )) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "longdash")  +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs(title = "Event Time Event Study", 
         subtitle = "Randomly Chosen Treatment Timing")

ggsave("data/output/plots/pretrend-event-time-es-fake.png", width = 8, height = 6)

pretrend_sim  %>%
    filter(type == "calendar") %>%
    group_by(time, vaccine) %>%
    summarise( 
        mean = mean(estimate), 
        conf.high = quantile(estimate, 0.975), 
        conf.low = quantile(estimate, 0.025)
    ) %>%
    ggplot(aes( 
        x = time, 
        y = mean, 
        ymin = conf.low, 
        ymax = conf.high
    )) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs( 
        title = "Permutation Test - Calendar Time Event Study"
    )

ggsave(
    "data/output/plots/perm-test-pretrends.png", 
    width = 8, height = 6
)


pretrend_sim  %>%
    filter(type == "calendar") %>%
    # filter(!(vaccine %in% c("Measles-1", "Measles-2"))) %>%
    group_by(time, vaccine) %>%
    summarise( 
        mean = median(estimate), 
    ) %>%
    ggplot(aes( 
        x = time, 
        y = mean
    )) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "longdash") +
    facet_wrap(~vaccine) +
    theme_bw() +
    labs( 
        title = "Permutation Test - Calendar Time ES Medians"
    )

ggsave(
    "data/output/plots/perm-test-pretrends-no-ci.png", 
    width = 8, height = 6
)
