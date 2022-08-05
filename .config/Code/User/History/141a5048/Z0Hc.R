library(tidyverse)
library(readxl)
library(ggstance)
library(metafor)
library(meta)

df = read_xlsx("data/gdrive-Wolf et al studies.xlsx", sheet = 1, skip = 1)

df = janitor::clean_names(df)

#' Impute Standard Errors based on CIs
#' 
#' We're only given confidence intervals not SEs so need to impute the SE given 
#' the CI. However, since this is RR CIs are calculated like so (I think:)
#' 
#'  CI = exp(log(RR) +- Z_{1 - \alpha/2} se(log(RR)))
#' Therefore, we can only uncover the se of log(RR) since exp(se(log(RR))) \neq 
#' se(RR).
#' 
#' When we do this, we can use either the upper or lower CI - I do both and plot 
#' imputed SEs against each other. I'm not sure why they don't align exactly in 
#' Wolf et al - perhaps due to rounding issues?
#'
impute_se = function(ci_data, bound) {
    if (bound == "upper") {
        ln_upper = log(ci_data$upper_95_percent_confidence_interval)
        ln_point = log(ci_data$effect_estimate_on_diarrhea)
        se_ln_RR = (ln_upper - ln_point)/1.96
    }
    if (bound == "lower") {
        ln_lower = log(ci_data$lower_95_percent_confidence_interval)
        ln_point = log(ci_data$effect_estimate_on_diarrhea)
        se_ln_RR = (ln_lower - ln_point)/1.96*-1
    }
    return(se_ln_RR)
}


df %>%
    select(
        upper_95_percent_confidence_interval,
        lower_95_percent_confidence_interval
    ) %>%
    View()

# Some initial cleaning
df = df %>%
    mutate(upper_95_percent_confidence_interval = as.numeric(upper_95_percent_confidence_interval)) %>%
    mutate(
        ref_first_word = str_extract(reference, "^\\w+"), 
        shorthand_ref = paste0(ref_first_word, "-", study_year),
        se_imp_upper = impute_se(., bound = "upper"),
        se_imp_lower = impute_se(., bound = "lower"),
        se_imp = (se_imp_upper + se_imp_lower)/2, 
        ln_RR = log(effect_estimate_on_diarrhea), 
    ) %>%
    group_by(shorthand_ref) %>%
    mutate(tmp_id = 1:n()) %>%
    ungroup() %>%
    mutate(shorthand_ref = paste0(shorthand_ref, "-", tmp_id)) %>%
    select(-tmp_id)
# This study has a point estimate outside it's own CI...
df = df %>%
    filter(
        !str_detect(
            reference,
            ". H. Humphrey, M. N. N. Mbuya, R. Ntozini, L. H. Moulton," ))
df %>%
    ggplot(aes( 
        x = effect_estimate_on_diarrhea, 
        xmin = lower_95_percent_confidence_interval,
        xmax = upper_95_percent_confidence_interval, 
        y =  shorthand_ref
    )) +
    geom_pointrangeh()


df %>%
    ggplot(aes( 
        x = se_imp_upper, 
        y = se_imp_lower
    )) +
    geom_point() +
    geom_abline(linetype = "longdash") +
    theme_minimal() +
    labs( 
        title = "Implied Standard Errors using Upper vs Lower CI"
    )
clean_df = df %>%
    filter( 
        !is.na(ln_RR),
        !is.na(se_imp)
    ) %>%
    mutate(study_number = 1:n())

chlorine_df = clean_df %>%
    filter(str_detect(intervention, "chlor"))

chlorine_df %>%
    ggplot(aes( 
        x = effect_estimate_on_diarrhea, 
        xmin = lower_95_percent_confidence_interval,
        xmax = upper_95_percent_confidence_interval, 
        y =  shorthand_ref
    )) +
    geom_pointrangeh()
## Saving data for Andrews Kasy pub bias
clean_df %>%
    select(
        shorthand_ref
        ) %>%
        write_csv("publication_bias/Applications/water/all-diarrhea-labels.csv")

clean_df %>%
    select( 
        ln_RR,
        se_imp, 
        study_number
    ) %>%
    write_csv("publication_bias/Applications/water/all-diarrhea-estimates.csv")



chlorine_df %>%
    select(
        shorthand_ref
        ) %>%
        write_csv("publication_bias/Applications/water/chlorine-diarrhea-labels.csv")

chlorine_df %>%
    select( 
        ln_RR,
        se_imp, 
        study_number
    ) %>%
    write_csv("publication_bias/Applications/water/chlorine-diarrhea-estimates.csv")


## Now some metanalysis stuff


init_fit = clean_df %>%
    rma(yi = ln_RR, vi = se_imp, data = .)

init_fit %>%
    forest()

init_fit %>%
    funnel()
init_fit %>%
    radial()
init_fit %>%
    qqnorm()
init_fit %>%
    regtest()
init_fit %>%
    trimfill()

cumul_init_fit = init_fit %>%
    cumul(order = clean_df$study_year)

forest(cumul_init_fit, atransf = exp)


?metabias
metabias(init_fit)
meta_fit = clean_df %>%
    metagen( 
        TE = ln_RR, 
        seTE = se_imp, 
        sm = "RR",
        data = .
    )


metabias(meta_fit)
meta_fit %>%
    funnel()
meta_fit %>%
    drapery()
# Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")

# Generate funnel plot (we do not include study labels here)
funnel.meta(meta_fit,
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)

# Add a legend
legend(x = 1.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)

# Add a title
title("Contour-Enhanced Funnel Plot (Third Wave Psychotherapies)")
