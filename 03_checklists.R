############################################
# Project:  MCLC Survey (2022)
# File: generate.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Save state contact info
# Generate state checklists
# Checklist for data, definitions, costs, notes

# Checklists created:
# state_data_checklist = checks for no data and if data doesn't add up
# previous_survey_checklist = check for changes in data between the two surveys
# adm_table_checklist = final admissions table that will be formatted in an email
# pop_table_checklist = final population table that will be formatted in an email
############################################

# create list containing each state's submission in google sheets
dfs <- list(Alabama, Idaho, Iowa)
dfs <- setNames(dfs,c("Alabama", "Idaho", "Iowa"))

# create a vector state names
states <- c("Alabama", "Idaho", "Iowa")

# run custom function that creates a list of data checklists for each state
# for example, if data doesn't add up correctly or they didn't input data
# accounts for misspellings of "na"
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing data value
state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_create_state_data_checklist(df_state, x)
})

# change list into a data frame
state_data_checklist <- bind_rows(state_data_checklist)

# merge with previous survey data to check for data changes for 2018-2020
# remove 2021 since we're comparing 2018-2020 between both 2021 and 2022 data collection
# remove variables not needed
previous_survey_checklist <- state_data_checklist %>%
  left_join(previous_survey, by = c("state", "year")) %>%
  filter(year != 2021) %>%
  select(state, year, everything()) %>%
  select(-c(check_other_prison_admissions_22:check_parole_violation_population_22))

# perform checks by seeinng if what was submitted last year is different from what was submitted this year for 2018-2020
# append 21_22 to variables so we know we are comparing 2021 survey to 2022 survey
previous_survey_checklist <- previous_survey_checklist %>%
  mutate(check_total_prison_admissions_21_22                    = case_when(total_prison_admissions_21                    == total_prison_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_supervision_violation_admissions_21_22     = case_when(total_supervision_violation_admissions_21     == total_supervision_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_probation_violation_admissions_21_22             = case_when(probation_violation_admissions_21             == probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),

         check_parole_violation_admissions_21_22                = case_when(parole_violation_admissions_21                == parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_technical_violation_admissions_21_22       = case_when(total_technical_violation_admissions_21       == total_technical_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_technical_probation_violation_admissions_21_22   = case_when(technical_probation_violation_admissions_21   == technical_probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_technical_parole_violation_admissions_21_22      = case_when(technical_parole_violation_admissions_21      == technical_parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_new_offense_admissions_21_22               = case_when(total_new_offense_admissions_21               == total_new_offense_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_probation_violation_admissions_21_22 = case_when(new_offense_probation_violation_admissions_21 == new_offense_probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_parole_violation_admissions_21_22    = case_when(new_offense_parole_violation_admissions_21    == new_offense_parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),

         check_total_prison_population_21_22                    = case_when(total_prison_population_21                    == total_prison_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_supervision_violation_population_21_22     = case_when(total_supervision_violation_population_21     == total_supervision_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_probation_violation_population_21_22             = case_when(probation_violation_population_21             == probation_violation_population_22 ~ "Same", TRUE ~ "Different"),

         check_parole_violation_population_21_22                = case_when(parole_violation_population_21                == parole_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_technical_violation_population_21_22       = case_when(total_technical_violation_population_21       == total_technical_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_technical_probation_violation_population_21_22   = case_when(technical_probation_violation_population_21   == technical_probation_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_technical_parole_violation_population_21_22      = case_when(technical_parole_violation_population_21      == technical_parole_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_new_offense_population_21_22               = case_when(total_new_offense_population_21               == total_new_offense_population_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_probation_violation_population_21_22 = case_when(new_offense_probation_violation_population_21 == new_offense_probation_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_parole_violation_population_21_22    = case_when(new_offense_parole_violation_population_21    == new_offense_parole_violation_population_22 ~ "Same", TRUE ~ "Different")
         )

# run custom function that creates an admissions table for email with data quality checks
# these final tables will include previously submitted data, new data, and columns for quality checks
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing data value
adm_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_adm_table_checklist(df_state, x)
})

# change list into a data frame
adm_table_checklist <- bind_rows(adm_table_checklist)

# run custom function that creates an population table for email with data quality checks
# these final tables will include previously submitted data, new data, and columns for quality checks
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing data value
pop_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_pop_table_checklist(df_state, x)
})

# change list into a data frame
pop_table_checklist <- bind_rows(pop_table_checklist)
