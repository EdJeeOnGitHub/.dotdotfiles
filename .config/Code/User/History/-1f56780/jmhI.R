library(tidyverse)
library(readxl)


raw_rural_df = read_xlsx("edited_Table_25_Final_Sindh_Urban_04 15 2022.xlsx", sheet = "Rural_original")


raw_rural_df = raw_rural_df[4:nrow(raw_rural_df), c(1, 2, 3, 4, 6, 11 )]

colnames(raw_rural_df) = c( 
    "T_number",
    "district", 
    "name",
    "habdast_number",
    "pop", 
    "lit"
) 

wip_rural_df = raw_rural_df %>%
    mutate( 
        variable_type = case_when(
            str_detect(name, "DISTRICT") ~ "district",
            str_detect(name, "TALUKA") ~ "taluka",
            str_detect(name, "STC") ~ "stc",
            str_detect(name, "(?<!S)TC") ~ "tc", 
            str_detect(name, "PC") ~ "tc", 
            str_detect(habdast_number, "000") ~ "village"
        ) 
    )  %>%
    select(district, name, pop, lit, variable_type)  %>%
    filter(!is.na(variable_type)) %>%
    mutate(
        taluka_id = cumsum(variable_type == "taluka"), 
        stc_id = cumsum(variable_type == "stc"), 
        tc_id = cumsum(variable_type == "tc"),
        district_id = district
    )

create_area_data = function(df, variable_type_string){
    var_id = rlang::sym(str_c(variable_type_string, "_id"))
    var_pop = rlang::sym(str_c(variable_type_string, "_pop"))
    var_lit = rlang::sym(str_c(variable_type_string, "_lit"))

    subset_df = df %>%
        filter(
            variable_type == variable_type_string
        ) %>%
        rename(
            !!var_pop := pop,
            !!var_lit := lit
        ) %>%
        select(all_of(var_id), all_of(var_pop), all_of(var_lit))
    return(subset_df)
}
district_df = wip_rural_df %>%
    create_area_data("district")

taluka_df = wip_rural_df %>%
    create_area_data("taluka")
stc_df = wip_rural_df %>%
    create_area_data("stc")
tc_df = wip_rural_df %>%
    create_area_data("tc")

village_df = wip_rural_df %>%
    filter(variable_type == "village")

rural_df = left_join( 
    village_df, 
    taluka_df, 
    by = "taluka_id"
) %>%
    left_join(
        stc_df,
        by = "stc_id"
    ) %>%
    left_join(
        tc_df,
        by = "tc_id"
    ) %>%
    mutate(across(c(contains("pop"), contains("lit")), as.numeric))


## Note these are problem cases
problem_cases = rural_df %>%
    group_by(taluka_id) %>%
    summarise( 
        sum_pop = sum(pop), 
        taluka_pop = unique(taluka_pop)
    ) %>%
    filter(sum_pop != taluka_pop)


write_csv(
    rural_df,
    "clean_rural_data.csv"
)







wip_rural_df %>%
    filter(
        variable_type == "village"
    )



string = "ed TC"


str_detect( 
    string,
    "(?<!S)TC"
)
