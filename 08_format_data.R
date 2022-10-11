############################################
# Project:  MCLC Survey (2022)
# File: format_data.R
# Last updated: October 11, 2022
# Author: Mari Roberts

# Format MCLC survey data for web team
############################################

##########
# Format data so each year and type (adm vs pop) is an excel sheet. E.g. Admissions 2021
##########

# extract data submissions
adm_new <- adm_table_checklist %>% select(state, metric,
                                           current_2018:current_2021)
pop_new <- pop_table_checklist %>% select(state, metric,
                                           current_2018:current_2021)

# replace completes and left blanks with no data
adm_new[adm_new == "Complete"] <- "No Data"
adm_new[adm_new == "Left Blank"] <- "No Data"
pop_new[pop_new == "Left Blank"] <- "No Data"

# create "sheet" for each adm data year
# same format as last year
adm_2018 <- adm_new %>% select(state, metric, current_2018)
adm_2018 <- spread(adm_2018, metric, current_2018)
adm_2018 <- fnc_org_adm_columns(adm_2018)

adm_2019 <- adm_new %>% select(state, metric, current_2019)
adm_2019 <- spread(adm_2019, metric, current_2019)
adm_2019 <- fnc_org_adm_columns(adm_2019)

adm_2020 <- adm_new %>% select(state, metric, current_2020)
adm_2020 <- spread(adm_2020, metric, current_2020)
adm_2020 <- fnc_org_adm_columns(adm_2020)

adm_2021 <- adm_new %>% select(state, metric, current_2021)
adm_2021 <- spread(adm_2021, metric, current_2021)
adm_2021 <- fnc_org_adm_columns(adm_2021)

# create "sheet" for each pop data year
# same format as last year
pop_2018 <- pop_new %>% select(state, metric, current_2018)
pop_2018 <- spread(pop_2018, metric, current_2018)
pop_2018 <- fnc_org_pop_columns(pop_2018)

pop_2019 <- pop_new %>% select(state, metric, current_2019)
pop_2019 <- spread(pop_2019, metric, current_2019)
pop_2019 <- fnc_org_pop_columns(pop_2019)

pop_2020 <- pop_new %>% select(state, metric, current_2020)
pop_2020 <- spread(pop_2020, metric, current_2020)
pop_2020 <- fnc_org_pop_columns(pop_2020)

pop_2021 <- pop_new %>% select(state, metric, current_2021)
pop_2021 <- spread(pop_2021, metric, current_2021)
pop_2021 <- fnc_org_pop_columns(pop_2021)

# write each data frame as a sheet into an excel workbook
mclc_data_2022 <- list('Admissions 2018' = adm_2018,
                       'Admissions 2019' = adm_2019,
                       'Admissions 2020' = adm_2020,
                       'Admissions 2021' = adm_2021,

                       'Population 2018' = pop_2018,
                       'Population 2019' = pop_2019,
                       'Population 2020' = pop_2020,
                       'Population 2021' = pop_2021)

write.xlsx(mclc_data_2022, file = "mclc_data_2022.xlsx")

##########
# Compare notes from last year to let comms know if they changed
##########





##########
# Save to sharepoint
##########

