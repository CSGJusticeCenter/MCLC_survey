############################################
# Project:  MCLC Survey (2022)
# File: checklists.R
# Last updated: August 21, 2022
# Author: Mari Roberts

# Checklists created:
# survey_checklist = checks for data left blank and if data doesn't add up correctly
# previous_survey_checklist = check for changes in data between the two surveys
# adm_table_checklist = final admissions table with qa checks (filter by state) that will be formatted in gt
# pop_table_checklist = final population table with qa checks (filter by state) that will be formatted in gt
# notes_comments_list = puts notes and comments in a table that will be formatted in gt
# contact_list = contact info by state
# definitions_table_checklist = table that shows definitions that weren't confirmed that will be formatted in gt
############################################

# create list containing each state's submission in google sheets
dfs <- list(Alabama, Idaho, Iowa, Pennsylvania)
dfs <- setNames(dfs,c("Alabama", "Idaho", "Iowa", "Pennsylvania"))

# create a vector state names
states <- c("Alabama", "Idaho", "Iowa", "Pennsylvania")

###########
# state_data_checklist
###########

# create empty list
df_final <- list()

# run custom function that creates a list of data checklists for each state
# for example, if data doesn't add up correctly or they left something blank
# accounts for misspellings of "na" but these should be checked periodically since-
# states will likely have more spelling variations of NA
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing value
state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_create_state_data_checklist(df_state, x)
})

# change list into a data frame and arrange columns
state_data_checklist <- bind_rows(state_data_checklist)
state_data_checklist <- state_data_checklist %>% select(state, year, everything())

# merge state_data_checklist with previous survey data to check for data changes for 2018-2020
# remove 2021 since we're comparing 2018-2020 between both 2021 and 2022 surveys
survey_checklist <- state_data_checklist %>%
  left_join(previous_survey, by = c("state", "year")) %>%
  filter(year != 2021) %>%
  select(state, year, everything())

# perform checks by seeing if what was submitted last year is different from what was submitted this year for 2018-2020
# for example, if total prison admissions in 2021 was different in 2022, then label total prison admissions as "different"
# append 21_22 to variables so we know we are comparing 2021 survey to 2022 survey
survey_checklist <- survey_checklist %>%
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

###########
# adm_table_checklist
###########

# run custom function that creates an admissions list (one df for each state) with data quality checks
# these final tables will include previously submitted data and new data
# ignore the warning message
adm_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_adm_table_checklist(df_state, x)
})

# change list into a data frame and arrange columns
adm_table_checklist <- bind_rows(adm_table_checklist)
adm_table_checklist <- adm_table_checklist %>% select(state, everything())

###########
# pop_table_checklist
###########

# run custom function that creates an population list (one df for each state) with data quality checks
# these final tables will include previously submitted data and new data
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing data value
pop_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_pop_table_checklist(df_state, x)
})

# change list into a data frame and arrange columns
pop_table_checklist <- bind_rows(pop_table_checklist)
pop_table_checklist <- pop_table_checklist %>% select(state, everything())

###########
# costs_table_checklist
###########

# run custom function that creates a cost list (one df for each state) with data quality checks
# ignore the warning message, it's about changing some values to NA
costs_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_costs(df_state, x)
})

# change list into a data frame
costs_table_checklist <- bind_rows(costs_table_checklist)

# rename variables and merge with cost data from previous survey
costs_table_checklist <- costs_table_checklist %>%
  left_join(previous_costs, by = c("state")) %>%
  select(state, previous_2019, previous_2020,
         current_2019 = year_2019,
         current_2020 = year_2020,
         current_2021 = year_2021)

# perform checks by seeing if what was submitted last year is different from what was submitted this year for 2018-2020
# append 21_22 to variables so we know we are comparing 2021 survey to 2022 survey
costs_table_checklist <- costs_table_checklist %>%
  mutate(check_2019_21_22 = case_when(current_2019 == previous_2019 ~ "Same", TRUE ~ "Different"),
         check_2020_21_22 = case_when(current_2020 == previous_2020 ~ "Same", TRUE ~ "Different"))

###########
# notes_comments_list
###########

# run custom function that creates a notes and comments list (one df for each state)
# no qa check for notes. they can see what was submitted and change it if they want to
# ignore warning message
notes_comments_list <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_notes_comments(df_state, x)
})

# change list into a data frame
notes_comments_list <- bind_rows(notes_comments_list)

###########
# contact_list
###########

# run custom function that creates a contact list (one df for each state)
# ignore error message
contact_list <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_contact_info(df_state, x)
})

# change list into a data frame
contact_list <- bind_rows(contact_list)

###########
# definitions_table_checklist
###########

# run custom function that creates a definitions checklist (one df for each state)
# checks to make sure the respondent checked the box next to the definitions
# ignore error message
definitions_table_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_definitions(df_state, x)
})

# change list into a data frame
definitions_table_checklist <- bind_rows(definitions_table_checklist)
# definitions_table_checklist <- definitions_table_checklist %>% filter(definition_confirmation == "Not Confirmed")
