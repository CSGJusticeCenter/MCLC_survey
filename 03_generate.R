############################################
# Project:  MCLC Survey (2022)
# File: generate.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Save state contact info
# Generate state checklists
# Checklist for data, definitions, costs, notes
############################################

# create list containing each state's submission in google sheets
dfs <- list(Alabama, Idaho, Iowa)
dfs <- setNames(dfs,c("Alabama", "Idaho", "Iowa"))
states <- c("Alabama", "Idaho", "Iowa")

# run custom function that creates a list of data checklists for each state
# for example, if data doesn't add up correctly, they didn't input data
# accounts for misspellings of "na"
state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_create_state_data_checklist(df_state, x)
})

# change list into a data frame
state_data_checklist <- bind_rows(state_data_checklist)

# merge with prevous survey data to check for data changes for 2018-2020
# remove 2021
previous_survey_checklist <- state_data_checklist %>%
  left_join(previous_survey, by = c("state", "year")) %>%
  filter(year != 2021) %>%
  select(state, year, everything()) %>%
  select(-c(check_other_prison_admissions:check_parole_violation_population))

previous_survey_checklist <- previous_survey_checklist %>%
  mutate(check_total_prison_admissions                    = case_when(prev_total_prison_admissions                    == total_prison_admissions ~ "same", TRUE ~ "different"),
         check_total_supervision_violation_admissions     = case_when(prev_total_supervision_violation_admissions     == total_supervision_violation_admissions ~ "same", TRUE ~ "different"),
         check_probation_violation_admissions             = case_when(prev_probation_violation_admissions             == probation_violation_admissions ~ "same", TRUE ~ "different"),

         check_parole_violation_admissions                = case_when(prev_parole_violation_admissions                == parole_violation_admissions ~ "same", TRUE ~ "different"),
         check_total_technical_violation_admissions       = case_when(prev_total_technical_violation_admissions       == total_technical_violation_admissions ~ "same", TRUE ~ "different"),
         check_technical_probation_violation_admissions   = case_when(prev_technical_probation_violation_admissions   == technical_probation_violation_admissions ~ "same", TRUE ~ "different"),
         check_technical_parole_violation_admissions      = case_when(prev_technical_parole_violation_admissions      == technical_parole_violation_admissions ~ "same", TRUE ~ "different"),
         check_total_new_offense_admissions               = case_when(prev_total_new_offense_admissions               == total_new_offense_admissions ~ "same", TRUE ~ "different"),
         check_new_offense_probation_violation_admissions = case_when(prev_new_offense_probation_violation_admissions == new_offense_probation_violation_admissions ~ "same", TRUE ~ "different"),
         check_new_offense_parole_violation_admissions    = case_when(prev_new_offense_parole_violation_admissions    == new_offense_parole_violation_admissions ~ "same", TRUE ~ "different"),

         check_total_prison_population                    = case_when(prev_total_prison_population                    == total_prison_population ~ "same", TRUE ~ "different"),
         check_total_supervision_violation_population     = case_when(prev_total_supervision_violation_population     == total_supervision_violation_population ~ "same", TRUE ~ "different"),
         check_probation_violation_population             = case_when(prev_probation_violation_population             == probation_violation_population ~ "same", TRUE ~ "different"),

         check_parole_violation_population                = case_when(prev_parole_violation_population                == parole_violation_population ~ "same", TRUE ~ "different"),
         check_total_technical_violation_population       = case_when(prev_total_technical_violation_population       == total_technical_violation_population ~ "same", TRUE ~ "different"),
         check_technical_probation_violation_population   = case_when(prev_technical_probation_violation_population   == technical_probation_violation_population ~ "same", TRUE ~ "different"),
         check_technical_parole_violation_population      = case_when(prev_technical_parole_violation_population      == technical_parole_violation_population ~ "same", TRUE ~ "different"),
         check_total_new_offense_population               = case_when(prev_total_new_offense_population               == total_new_offense_population ~ "same", TRUE ~ "different"),
         check_new_offense_probation_violation_population = case_when(prev_new_offense_probation_violation_population == new_offense_probation_violation_population ~ "same", TRUE ~ "different"),
         check_new_offense_parole_violation_population    = case_when(prev_new_offense_parole_violation_population    == new_offense_parole_violation_population ~ "same", TRUE ~ "different")
         )

# Admissions
# Previous checklist
prev_alabama_adm <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, prev_total_prison_admissions:prev_new_offense_parole_violation_admissions) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate_if(is.character, funs(ifelse(is.na(.), "left blank or no data", .)))
prev_alabama_pop_check <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, check_total_prison_admissions:check_new_offense_parole_violation_admissions)

# Current checklist
current_alabama_adm <- state_data_checklist %>%  filter(state == "Alabama") %>%
  select()

# Population
# Previous checklist
prev_alabama_pop <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, prev_total_prison_population:prev_new_offense_parole_violation_population) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate_if(is.character, funs(ifelse(is.na(.), "left blank or no data", .)))
prev_alabama_pop_check <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, check_total_prison_population:check_new_offense_parole_violation_population)
# Current checklist
state_data_checklist

prev_alabama_adm <- dcast(melt(prev_alabama_adm, id.vars = "year"), variable ~ year)
prev_alabama_adm <- prev_alabama_adm %>%
  clean_names() %>%
  rename(current_2018 = x2018,
         current_2019 = x2019,
         current_2020 = x2020) %>%
  filter(variable != "state") %>%
  rename(metric = variable) %>%
  mutate(current_2018 = comma(current_2018, digits = 0),
         current_2019 = comma(current_2019, digits = 0),
         current_2020 = comma(current_2020, digits = 0),
         metric = str_replace(metric, "prev_", "")) %>%
  mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
  mutate(metric = str_to_title(metric))

prev_alabama_adm_check <- dcast(melt(prev_alabama_adm_check, id.vars = "year"), variable ~ year)
prev_alabama_adm_check <- prev_alabama_adm_check %>%
  clean_names() %>%
  rename(prev_2018 = x2018,
         prev_2019 = x2019,
         prev_2020 = x2020) %>%
  filter(variable != "state") %>%
  select(-variable)

prev_alabama_adm_all <- cbind(prev_alabama_adm, prev_alabama_adm_check)
