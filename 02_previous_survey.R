############################################
# Project:  MCLC Survey (2022)
# File: previous_survey.R
# Last updated: August 10, 2022
# Author: Mari Roberts

# Format data from previous MCLC survey to check for data submission changes
# in the new form submitted through google sheet
# File: Data for web team 2021 v13.xlsx
############################################

# add year variable
adm18$year <- "2018"
adm19$year <- "2019"
adm20$year <- "2020"
pop18$year <- "2018"
pop19$year <- "2019"
pop20$year <- "2020"

# add data together
adm <- rbind(adm18, adm19, adm20)
pop <- rbind(pop18, pop19, pop20)

# clean names
# rename variable
adm <- clean_names(adm) %>% rename(state = states)
pop <- clean_names(pop) %>% rename(state = states)

# add adm and pop data together
adm_pop <- merge(adm, pop, by = c("state", "state_abbrev", "year"))

# remove state abbrevs
# change data types
# calculate difference between total and supervision violations to get number of "other"
adm_pop <- adm_pop %>%
  select(-state_abbrev) %>%
  mutate(state = factor(state)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(other_admissions = total_admissions-total_violation_admissions,
         other_population = total_population-total_violation_population,
         total_new_offense_admissions = new_offense_parole_violation_admissions + new_offense_probation_violation_admissions,
         total_new_offense_population = new_offense_parole_violation_population + new_offense_probation_violation_population,
         total_technical_violation_admissions = technical_parole_violation_admissions + technical_probation_violation_admissions,
         total_technical_violation_population = technical_parole_violation_population + technical_probation_violation_population)

# subset for now
previous_survey <- adm_pop %>% filter(state == "Alabama" | state == "Idaho" | state == "Iowa" )

# rename variables so they're consistent with new survey
# change all data to characters
# if data left blank or NA, indicate with "Left Blank or No Data"
# add "previous survey" indicator to metrics
# this way we can compare what was submitted last year, e.g. if they changed any numbers from 2018-2020
# append "21" to variables so we know these metrics are from the 2021 survey
previous_survey <- previous_survey %>%

  select(state,
         year,
         total_prison_admissions                = total_admissions,
         total_supervision_violation_admissions = total_violation_admissions,
         probation_violation_admissions         = total_probation_violation_admissions,
         parole_violation_admissions            = total_parole_violation_admissions,
         total_technical_violation_admissions,
         technical_probation_violation_admissions,
         technical_parole_violation_admissions,
         total_new_offense_admissions,
         new_offense_probation_violation_admissions,
         new_offense_parole_violation_admissions,

         total_prison_population                = total_population,
         total_supervision_violation_population = total_violation_population,
         probation_violation_population         = total_probation_violation_population,
         parole_violation_population            = total_parole_violation_population,
         total_technical_violation_population,
         technical_probation_violation_population,
         technical_parole_violation_population,
         total_new_offense_population,
         new_offense_probation_violation_population,
         new_offense_parole_violation_population) %>%

  mutate(across(everything(), as.character)) %>%
  mutate_if(is.character, dplyr::funs(ifelse(is.na(.), "Left Blank or No Data", .))) %>%
  rename_with(~ paste0(., "_21"), -c(state, year)) %>%
  mutate(year = as.numeric(year))
