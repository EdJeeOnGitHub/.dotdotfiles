library(tidyverse)
library(readxl)
library(janitor)
library(lme4)



df = read_xlsx(
    "code/hh-power-sims/v3 coverages by EPI targets-20210909.xlsx", 
    sheet = 3, 
    skip = 2) %>%
    clean_names() %>%
    filter(row_number() > 1) %>%
    select(1:5)

cols = c(
    "district", 
    "uc",
    "estim_births",
    "n_vaxxed",
    "pct_vaxxed"
)
colnames(df) = cols

df = df %>%
    mutate( 
        across(
            .cols = c("estim_births",
                      "n_vaxxed",
                      "pct_vaxxed"), 
            as.numeric 
            )
    ) 
df %>%
    filter(!str_detect(
        district, 
        "Source|There is|Annual live|Row Labels"
    )) %>%
    filter(!is.na(district))
    filter(is.na(estim_births))


# now as a function
clean_uc_vax_rate = function(file_path,
                             sheet,
                             cols = c("district", 
                                      "uc", 
                                      "estim_births", 
                                      "n_vaxxed", 
                                      "pct_vaxxed")){
   raw_df = read_xlsx(
        "code/hh-power-sims/v3 coverages by EPI targets-20210909.xlsx", 
        sheet = sheet, 
        skip = 2) %>%
        clean_names() %>%
        filter(row_number() > 1) %>%
        filter(row_number() < 1107) %>%
        select(1:5)
    colnames(raw_df) = cols
    df = raw_df %>%
        filter(!str_detect(
            district, 
            "Source|There is|Annual|Row Labels"
        )) %>%
        filter(!is.na(district)) %>%
        filter(!str_detect(uc, "UC 08 Malir Cantt")) %>%
            mutate( 
                across(
                    .cols = c("estim_births",
                            "n_vaxxed",
                            "pct_vaxxed"), 
                    as.numeric 
                    )
            )
    return(df)
}



clean_uc_vax_rate(
    file_path = "code/hh-power-sims/v3 coverages by EPI targets-20210909.xlsx", 
    sheet = 3
)


vaccines = c(
    "bcg",
    "penta1",
    "penta2",
    "penta3",
    "measles1",
    "measles2"
)

sheets = c(3, 5, 7, 9, 11, 13)



clean_vax_dfs = map2_dfr(
    sheets,
    vaccines,
    ~clean_uc_vax_rate(
        file_path = "code/hh-power-sims/v3 coverages by EPI targets-20210909.xlsx",
        sheet = .x
    ) %>% 
    mutate(vaccine = .y)
) 


clean_vax_dfs %>%
    lmer( 
        data = ., 
        formula = pct_vaxxed ~ (1 | district) + (1| uc) + (1 | vaccine)
    )

clean_vax_dfs %>%
    select(district) %>%
    unique()

districts = c(
    "Kambar",
    "Karachi East",
    "Karachi West",
    "Sujawal"
)

aov_summaries = map(
    vaccines, 
    ~summary(
        aov(
            pct_vaxxed ~ district, 
            data = clean_vax_dfs %>% filter(vaccine == .x)
        )
    )
)


aov_icc_func = function(summ){
    summ[[1]][1,2]/sum(summ[[1]][,2])
}

iccs = map(
    aov_summaries,
    aov_icc_func
) %>% unlist() 

names(iccs) = vaccines
min(
    iccs
)
max(iccs)
mean(iccs)
aov_summaries
summary_aov = summary(
    aov(
        pct_vaxxed ~  district,
        data = clean_vax_dfs %>% filter(district %in% districts, vaccine == "bcg")
        )
    )

summary_aov

aov_icc_func(summary_aov)


