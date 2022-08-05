library(tidyverse)
library(fixest)
library(broom)

predict_partial <- function(object, newdata, se.fit = FALSE,
                            interval = "none",
                            level = 0.95){
    if(missing(newdata)) {
        stop("predict_partial requires newdata and predicts for all group effects = 0.")
    }
    
    # Extract terms object, removing response variable 
    tt <- terms(object)
    Terms <- delete.response(tt)
    
    # Remove intercept
    attr(Terms, "intercept") <- 0
    
    X <- model.matrix(Terms, data = newdata)
    
    if (class(object) == "fixest") {
        B <- as.numeric(coef(object))
        df <- attributes(vcov(object, attr = T))$dof.K
    } else if (class(object) %in% c("lm", "felm")) { 
        B <- as.numeric(object$coef)
        df <- object$df.residual
    } else {
        stop("class(object) should be lm, fixest, or felm.")
    }
    
    fit <- data.frame(fit = as.vector(X %*% B))
    
    if(se.fit | interval != "none") {
        sig <- vcov(object)
        se <- apply(X, MARGIN = 1, FUN = get_se, sig = sig)
    }
    
    if(interval == "confidence"){
        t_val <- qt((1 - level) / 2 + level, df = df)
        fit$lwr <- fit$fit - t_val * se
        fit$upr <- fit$fit + t_val * se
    } else if (interval == "prediction"){
        stop("interval = \"prediction\" not yet implemented")
    }
    if(se.fit){
        return(list(fit=fit, se.fit = se))
    } else {
        return(fit)
    }
}

get_se <- function(r, sig) {
    # Compute linear combination, helper function for predict_partial
    # Given numeric vector r (the constants) and vcov sig (the ), compute SE 
    r <- matrix(r, nrow = 1)
    sqrt(r %*% sig %*% t(r))
}

raw_df = read_csv("va-data.csv", guess_max = 100000)



fit_1 = feols(
    fml = score_std ~ lag_score_std + hh_income + m_education | id_teacher + id_school^id_grade^id_class^id_cohort^year,
    data = raw_df
)


predict_partial(
    fit_1,
    newdata = raw_df %>%
        select(-id_teacher)
)




beta = tidy(fit_1)  %>%  
    select(estimate) %>%
    pull() %>%
    as.matrix(ncol = 1)


summary(fit_1)

X = raw_df %>%
    select(lag_score_std, hh_income, m_education) %>%
    as.matrix()


A_hat = X %*% beta
raw_df = raw_df %>%
    mutate( 
        v_ijt = score_std - A_hat[, 1]
    )
summ_df = raw_df %>%
    group_by(
        id_teacher, year
    ) %>%
    summarise( 
        v_jt = mean(v_ijt, na.rm = TRUE), 
        .groups = "drop"
    )

summ_df %>%
    filter(!is.nan(v_jt)) %>%
    pivot_wider( 
        id_cols = id_teacher,
        names_from = year,
        values_from = v_jt, 
        names_prefix = "v_year_"
    )

estim_va_year = function(data, year) {
    subset_data = data %>%
        filter(!is.nan(v_jt) & year <= year)

    y_var = paste0("v_year_", year)
    reg_formula = as.formula(paste0(y_var, " ~ 0 + . -id_teacher"))

    wide_subset_data = subset_data %>%
        pivot_wider( 
            id_cols = id_teacher,
            names_from = year,
            values_from = v_jt, 
            names_prefix = "v_year_"
        )

    mu_hat = predict(lm(
        data = wide_subset_data,
        formula = reg_formula,
        na.action = na.exclude
    ), na.action = na.exclude)
    pred_df = tibble(
        value_added = mu_hat,
        id_teacher = wide_subset_data$id_teacher,
        year = year
    )

    return(pred_df)
}


unique(raw_df$year)

years = 2001:2007


va_df = map_dfr(
    years,
    ~estim_va_year(data = summ_df, year = .x))
va_df = va_df %>%
    group_by(year) %>%
    mutate(value_added_scaled = scale(value_added) %>% as.numeric())


va_df %>%
    group_by( 
        id_teacher
    ) %>%
    summarise(
        mean_va = mean(value_added_scaled)
    ) %>%
    arrange(desc(mean_va)) %>%
    select(id_teacher) %>%
    slice(1:10) %>%
    pull()


