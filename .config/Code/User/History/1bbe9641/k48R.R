library(tidyverse)
library(readxl)
library(janitor)



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
        select(1:5)
    colnames(raw_df) = cols
    df = raw_df %>%
        filter(!str_detect(
            district, 
            "Source|There is|Annual live|Row Labels"
        )) %>%
        filter(!is.na(district)) %>%
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

