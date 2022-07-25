############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: July 25, 2022
# Author: Mari Roberts

# Custom functions
############################################

library(dplyr)
library(janitor)
library(stringr)


#################
# custom function to extract name and email
#################

fnc_contact_info <- function(df, state_name){
  name  <- df[20,1]
  email <- df[20,3]
  df1 <- as.data.frame(c(name, email))
  df1 <- df1 %>% mutate(state_name = state_name) %>%
    rename(name = 1,
           email = 2)
}

#################
# custom function to generate state data checklist
# for example, if numbers add up, what was left blank, etc.
#################

fnc_create_state_data_checklist <- function(df, state_name){

  # clean variable names
  df1 <- janitor::clean_names(df)

  # combine columns of text
  df1$metric <- apply(df1[,1:2], 1, function(x) x[!is.na(x)][1])

  # select data area in spreadsheet (remove white space and instructions that imported from Google Sheet)
  df_numbers <- df1 %>% select(metric,
                               year_2018 = x4,
                               year_2019 = x5,
                               year_2020 = x6,
                               year_2021 = x7)
  df_numbers <- df_numbers[c(24:33, 36:45),]

  # if data left blank, indicate with "left blank" - only works if all numeric
  df_numbers <- df_numbers %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), "left blank", .)))

  # change all data to characters
  df_numbers[] <- as.data.frame(lapply(df_numbers, as.character))

  # make all data lowercase
  # replace NAs and nulls (blanks) to "left blank"
  # indicate various ways to spell NA as "no data"
  df_numbers <- df_numbers %>%
    mutate_if(is.character, str_to_lower) %>%

    mutate_if(is.character, funs(ifelse(is.na(.), "left blank", .))) %>%
    mutate_if(is.character, ~replace(., . == "null", "left blank")) %>%

    mutate_if(is.character, ~replace(., . == "na", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "n/a", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "nodata", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "no data", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "not available", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "notavailable", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "notready", "no data")) %>%
    mutate_if(is.character, ~replace(., . == "not ready", "no data"))

  # remove all blank spaces and punctuation - NEED???????????????????
  # df_numbers <- apply(df_numbers, 2, str_remove_all, " ")
  # df_numbers <- apply(df_numbers,2,function(x)gsub('\\s+', '',x))
  # df_numbers <- as.data.frame(apply(df_numbers,2,function(x)gsub('\\s+', '',x)))

  # transpose data
  df_numbers <- as.data.frame(t(df_numbers))

  # make first row header, get year and make all text lower case
  df_numbers <- df_numbers %>%
    row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("year") %>%
    janitor::clean_names() %>%
    mutate(year = case_when(grepl("2018", year) ~ 2018,
                            grepl("2019", year) ~ 2019,
                            grepl("2020", year) ~ 2020,
                            grepl("2021", year) ~ 2021))


  # check to see if numbers add up correctly
  # supervision violations = probation + parole violations
  # probation violations = technical probation + new offense probation
  # parole violations = technical parole + new offense parole
  df_numbers <- df_numbers %>%
    mutate(check_other_prison_admissions = as.numeric(total_prison_admissions) - as.numeric(supervision_violation_admissions),
           check_other_prison_population = as.numeric(total_prison_population) - as.numeric(supervision_violation_population),

           check_supervision_violation_admissions = case_when(as.numeric(supervision_violation_admissions) == as.numeric(probation_violation_admissions) + as.numeric(parole_violation_admissions) ~ "correct",
                                                              supervision_violation_admissions == "no data" ~ "no data",
                                                              supervision_violation_admissions == "left blank" ~ "left blank",
                                                              TRUE ~ "doesn't add up"),
           check_probation_violation_admissions   = case_when(as.numeric(probation_violation_admissions) == as.numeric(technical_probation_violation_admissions) + as.numeric(new_offense_probation_admissions) ~ "correct",
                                                              probation_violation_admissions == "no data" ~ "no data",
                                                              probation_violation_admissions == "left blank" ~ "left blank",
                                                              TRUE ~ "doesn't add up"),
           check_parole_violation_admissions      = case_when(as.numeric(parole_violation_admissions) == as.numeric(technical_parole_violation_admissions) + as.numeric(new_offense_parole_admissions) ~ "correct",
                                                              parole_violation_admissions == "no data" ~ "no data",
                                                              parole_violation_admissions == "left blank" ~ "left blank",
                                                              TRUE ~ "doesn't add up"),
           check_probation_violation_population   = case_when(as.numeric(probation_violation_population) == as.numeric(technical_probation_violation_population) + as.numeric(new_offense_probation_population) ~ "correct",
                                                              probation_violation_population == "no data" ~ "no data",
                                                              probation_violation_population == "left blank" ~ "left blank",
                                                              TRUE ~ "doesn't add up"),
           check_parole_violation_population      = case_when(as.numeric(parole_violation_population) == as.numeric(technical_parole_violation_population) + as.numeric(new_offense_parole_population) ~ "correct",
                                                              parole_violation_population == "no data" ~ "no data",
                                                              parole_violation_population == "left blank" ~ "left blank",
                                                              TRUE ~ "doesn't add up"),
           state = state_name)
  return(df_numbers)
}



