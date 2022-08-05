## Clean the raw ZM data dump
library(haven)
library(tidyverse)
library(data.table)

raw_dt = read_dta("data/input/Dummy Data_ZM (1).dta") %>%
    as.data.table()

cols_we_want = str_detect(
    colnames(raw_dt), 
    "^bcg|penta|measles|childid"
)
wip_dt = raw_dt[, ..cols_we_want]
# vaccinated => vaxxed
# retro => vaccinated somewhere else and know date
# retro_date_missing => vaxxed elsewhere, don't know the date
# outreach => vaccinated in outreach programme
# scheduled => scheduled, date missing  

vaccines = c(
    "bcg", 
    "measles1", 
    "measles2", 
    "penta1", 
    "penta2"
)
col_variable_type = c(
    "vaccinated",
    "vaccinationstatus", 
    "vaccinationagedays",
    "vaccinationeventtype", 
    "district",
    "town",
    "uc",
    "centername",
    "vaccinator",
    "vaccinationduedate",
    "vaccinationdate",
    "due_date",
    "vac_date",
    "timelinessfactor",
    "approvedreminder",
    "eventtype",
    "vaccinereminder1status",
    "vaccinereminder2status",
    "vaccinereminder3status",
    "vaccinereminder1duedate",
    "vaccinereminder2duedate",
    "vaccinereminder3duedate"
)

wip_dt[, 
    (str_c(vaccines, "vaccinated")) := lapply(.SD, 
                         str_detect, 
                         "BCG|Measles|Penta"), 
    .SDcols = vaccines]
melt_vars = map(col_variable_type, ~str_c(vaccines, .x))


long_vax_dt = melt(
    wip_dt, 
    id.vars = "childid",
    measure.vars =  melt_vars, 
    value.name = col_variable_type
)
long_vax_dt[, vaccine := vaccines[variable]]



create_aggregrate_data = function(data, var, group_vars){
    agg_dt = data[,
    .(agg_var = sum(get(var))),
    by = group_vars]
    setnames(agg_dt, "agg_var", var)
    return(agg_dt)
}

long_vax_dt[, 
    .(n_vax = sum(vaccinated)), 
    by = c("vaccine", "vac_date", "district")]
    
long_town_vax_dt = create_aggregrate_data(
    data = long_vax_dt,
    var = "vaccinated",
    group_vars = c("vaccine", "vac_date", "district", "town")
)

long_district_vax_dt = create_aggregrate_data(
    data = long_vax_dt,
    var = "vaccinated", 
    group_vars = c("vaccine", "vac_date", "district")
)






ed


town_dt = wip_dt[, 
                 .(vaccines = sum(.SD)), 
                 .SDcols = vaccines, 
                 by =   ]


raw_df %>%
    colnames()
raw_df %>%
    select( 
        childid, 
        contains("district"), 
        contains("town"), 
        contains("")

    )

raw_df %>%
    select( 
        bcgvaccinationdate, 
        bcgvac_date
    )
colnames(raw_df)
raw_df %>%
    group_by(bcgdistrict, bcgvaccinationdate) %>%
    summarise( 
        district_n = n()
    ) %>%
    mutate( 
        bcgvaccinationdate = lubridate::ymd_hms(bcgvaccinationdate)
    ) %>%
    ggplot(aes( 
        x = bcgvaccinationdate, 
        y = district_n, 
        colour = bcgdistrict
    )) + 
    geom_line()


raw_df %>%
    select(contains("bcg")) %>%
    View()
raw_df %>%
    colnames()
raw_df %>%
    View()

raw_df %>%
    View()

raw_dt = fread("relative-path-to-data.data")
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)


install_cmdstan()
check_cmdstan_toolchain(fix = TRUE)
