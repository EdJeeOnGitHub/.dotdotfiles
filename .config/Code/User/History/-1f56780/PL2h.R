## Code to clean census data
library(tidyverse) # data manipulation library
library(readxl) # library to load XLSX files

# Read in the data
raw_rural_df = read_xlsx("edited_Table_25_Final_Sindh_Urban_04 15 2022.xlsx", sheet = "Rural_original")
raw_urban_df = read_xlsx("edited_Table_25_Final_Sindh_Urban_04 15 2022.xlsx", sheet = "Urban_original")
# Keep cols we wawnt and skip first 4 rows
raw_urban_df = raw_urban_df[4:nrow(raw_urban_df), c(1, 2, 3, 7, 11)]
raw_rural_df = raw_rural_df[4:nrow(raw_rural_df), c(1, 2, 3, 4, 6, 11 )]
# creat column names
colnames(raw_urban_df) = c(
    "T_number",
    "district", 
    "name",
    "pop", 
    "lit"
) 
colnames(raw_rural_df) = c( 
    "T_number",
    "district", 
    "name",
    "habdast_number",
    "pop", 
    "lit"
) 

raw_rural_df %>%
    select(name, pop)

fake_df = tibble(mtcars)
fake_df 
fake_df %>%
    mutate( 
        ed_var = cyl + disp
    )


fake_df %>%
    mutate(mean_mpg = mean(mpg)) %>%
    select(mean_mpg)


fake_df %>%
    group_by(cyl) %>%
    mutate(mean_mpg = mean(mpg)) %>%
    select(mean_mpg)


summary_car_data = fake_df %>%
    group_by(cyl) %>%
    summarise(
        mean_mpg = mean(mpg), 
        sd_mpg = sd(mpg), 
        t_stat = mean_mpg/sd_mpg) 

summary_car_data
fake_df

summary_car_data

fake_df = fake_df %>%
    mutate(ed = cyl*disp, .) %>%
    mutate(ed_2 = 10*ed)


fake_df[1:7, "mpg"]
fake_df[x_idx,]


fake_df %>%
    select(mpg)
fake_df %>%
    filter(mpg == 21)

fake_df[fake_df$mpg == 21, ]
raw_rural_df

str_detect("astring bla", pattern = "blo")


raw_urban_df %>%
    # identify each r
    mutate( 
        variable_type = case_when(
            str_detect(name, "DISTRICT|DISTRI") & !str_detect(name, "MUNIC") ~ "district",
            str_detect(name, "TALUKA|TALU") ~ "taluka",
            str_detect(name, "CHARGE") ~ "charge",
            str_detect(name, "CIRCLE") ~ "circle",
            str_detect(name, "DIVISION") ~ "subdivision",
            str_detect(name, "MC") ~ "mc",
            str_detect(name, "STC") ~ "stc",
            str_detect(name, "(?<!S)TC") ~ "tc", 
            str_detect(name, "PC") ~ "tc"
        ) 
    ) %>%
    select(district, name, pop, lit, variable_type)  %>%
    filter(!is.na(variable_type)) %>%
    mutate( 
        taluka_tmp = (variable_type == "taluka")*1, 
        cs_t = cumsum(taluka_tmp)
    )


wip_urban_df = raw_urban_df %>%
    # identify each r
    mutate( 
        variable_type = case_when(
            str_detect(name, "DISTRICT|DISTRI") & !str_detect(name, "MUNIC") ~ "district",
            str_detect(name, "TALUKA|TALU") ~ "taluka",
            str_detect(name, "CHARGE") ~ "charge",
            str_detect(name, "CIRCLE") ~ "circle",
            str_detect(name, "DIVISION") ~ "subdivision",
            str_detect(name, "MC") ~ "mc",
            str_detect(name, "STC") ~ "stc",
            str_detect(name, "(?<!S)TC") ~ "tc", 
            str_detect(name, "PC") ~ "tc"
        ) 
    ) %>%
    select(district, name, pop, lit, variable_type)  %>%
    filter(!is.na(variable_type))  %>%
    mutate(
        district_id = district, 
        subdivision_id = cumsum(variable_type == "subdivision"),
        taluka_id = cumsum(variable_type == "taluka"), 
        charge_id = cumsum(variable_type == "charge"), 
        circle_id = cumsum(variable_type == "circle"), 
        mc_id = cumsum(variable_type == "mc"),
        tc_id = cumsum(variable_type == "tc")
    )


wip_rural_df = raw_rural_df %>%
    mutate( 
        variable_type = case_when(
            str_detect(name, "DISTRICT") & !str_detect(name, "MUNIC") ~ "district",
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

wip_rural_df
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


urban_district_df = wip_urban_df %>%
    create_area_data("district")
urban_charge_df = wip_urban_df %>%
    create_area_data("charge")
urban_charge_df
urban_subdivision_df = wip_urban_df %>%
    create_area_data("subdivision")
urban_circle_df = wip_urban_df %>%
    filter(variable_type == "circle")



urban_df = left_join(
    urban_circle_df, 
    urban_district_df, 
    by = "district_id"
) %>%
    left_join(
        urban_charge_df,
        by = "charge_id"
    ) %>%
    left_join(
        urban_subdivision_df,
        by = "subdivision_id"
    ) %>%
    mutate(across(contains("pop"), ~str_remove_all(pop, ","))) %>%
    mutate(across(c(contains("pop"), contains("lit")), as.numeric))

if (nrow(urban_df) != nrow(urban_circle_df)) {
    stop("Urban join failed - additional rows added.")
}


rural_district_df = wip_rural_df %>%
    create_area_data("district")
rural_district_df %>%
    group_by(district_id) %>%
    summarise(n = n()) %>%
    filter(n > 1)
rural_taluka_df = wip_rural_df %>%
    create_area_data("taluka")
rural_stc_df = wip_rural_df %>%
    create_area_data("stc")
rural_tc_df = wip_rural_df %>%
    create_area_data("tc")

rural_village_df = wip_rural_df %>%
    filter(variable_type == "village")

rural_df = left_join( 
    rural_village_df, 
    rural_taluka_df, 
    by = "taluka_id"
) %>%
    left_join(
        rural_stc_df,
        by = "stc_id"
    ) %>%
    left_join(
        rural_tc_df,
        by = "tc_id"
    ) %>%
    mutate(across(c(contains("pop"), contains("lit")), as.numeric))

if (nrow(rural_df) != nrow(rural_village_df)) {
    stop("Rural join failed, more rows added.")
}


## Note these are problem cases
rural_problem_cases = rural_df %>%
    group_by(taluka_id) %>%
    summarise( 
        sum_pop = sum(pop), 
        taluka_pop = unique(taluka_pop)
    ) %>%
    filter(sum_pop != taluka_pop)



urban_problem_cases = urban_df %>%
    group_by(district_id) %>%
    summarise( 
        sum_pop = sum(pop), 
        district_pop = unique(district_pop)
    ) %>%
    filter(sum_pop != district_pop)






write_csv(
    urban_df,
    "clean_urban_data.csv"
)

write_csv(
    rural_df,
    "clean_rural_data.csv"
)





