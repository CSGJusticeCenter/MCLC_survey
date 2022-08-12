############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: August 2, 2022
# Author: Mari Roberts

# Custom functions
############################################

#################
# Custom function to extract name and email
#################

fnc_contact_info <- function(df, state_name){
  name  <- df[20,1]
  email <- df[20,3]
  df1 <- as.data.frame(c(name, email))
  df1 <- df1 %>% mutate(state_name = state_name) %>%
    rename(name = 1,
           email = 2)
}

clean_titles <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
  x
}

#################
# Custom function to generate state data checklist
# If numbers add up, what was Left Blank, No Data
#################

fnc_create_state_data_checklist <- function(df, state_name){

  # clean variable names
  df1 <- janitor::clean_names(df)

  # combine columns of text
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])

  # select admissions and population data in spreadsheet
  # rename variables
  # remove white space and instructions that imported from Google Sheets
  df_numbers <- df1 %>% select(metric,
                               year_2018 = x5,
                               year_2019 = x6,
                               year_2020 = x7,
                               year_2021 = x8)
  df_numbers <- df_numbers[c(22:31, 34:43),]

  # make all data lowercase
  # if data Left Blank or says NULL, indicate with "Left Blank"
  # change all data to characters
  # indicate various ways to spell NA as "No Data"
  df_numbers <- df_numbers %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank", .))) %>%
    mutate_if(grepl('null',.), ~replace(., grepl('null', .), "Left Blank")) %>%
    mutate(across(everything(), ~replace(., . ==  "na" |
                                           . ==  "nodata" |
                                           . ==  "no data" |
                                           . ==  "notavailable" |
                                           . ==  "not available" |
                                           . ==  "notready" |
                                           . ==  "not ready" |
                                           . == "[none]" |
                                           . == "none"
                                         , "No Data")))

  # transpose data
  df_transposed <- as.data.frame(t(df_numbers))

  # make first row header and get year
  df_transposed <- df_transposed %>%
    row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("year") %>%
    janitor::clean_names() %>%
    mutate(year = case_when(grepl("2018", year) ~ 2018,
                            grepl("2019", year) ~ 2019,
                            grepl("2020", year) ~ 2020,
                            grepl("2021", year) ~ 2021)) %>%
    rename_with(~ paste0(., "_22"), -c(year))


  # check to see if numbers add up correctly
  # supervision violations = probation + parole violations
  # probation violations = technical probation + new offense probation
  # parole violations = technical parole + new offense parole
  df_final <- df_transposed %>%
    mutate(check_other_prison_admissions_22 = as.numeric(total_prison_admissions_22) - as.numeric(total_supervision_violation_admissions_22),
           check_other_prison_population_22 = as.numeric(total_prison_population_22) - as.numeric(total_supervision_violation_population_22),

           check_supervision_violation_admissions_22 = case_when(as.numeric(total_supervision_violation_admissions_22) == as.numeric(probation_violation_admissions_22) + as.numeric(parole_violation_admissions_22) ~ "Correct",
                                                              total_supervision_violation_admissions_22 == "No Data" ~ "No Data",
                                                              total_supervision_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                              TRUE ~ "Doesn't Add Up"),
           check_probation_violation_admissions_22   = case_when(as.numeric(probation_violation_admissions_22) == as.numeric(technical_probation_violation_admissions_22) + as.numeric(new_offense_probation_violation_admissions_22) ~ "Correct",
                                                              probation_violation_admissions_22 == "No Data" ~ "No Data",
                                                              probation_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                              TRUE ~ "Doesn't Add Up"),
           check_parole_violation_admissions_22      = case_when(as.numeric(parole_violation_admissions_22) == as.numeric(technical_parole_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Correct",
                                                              parole_violation_admissions_22 == "No Data" ~ "No Data",
                                                              parole_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                              TRUE ~ "Doesn't Add Up"),
           check_probation_violation_population_22   = case_when(as.numeric(probation_violation_population_22) == as.numeric(technical_probation_violation_population_22) + as.numeric(new_offense_probation_violation_population_22) ~ "Correct",
                                                              probation_violation_population_22 == "No Data" ~ "No Data",
                                                              probation_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                              TRUE ~ "Doesn't Add Up"),
           check_parole_violation_population_22      = case_when(as.numeric(parole_violation_population_22) == as.numeric(technical_parole_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
                                                              parole_violation_population_22 == "No Data" ~ "No Data",
                                                              parole_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                              TRUE ~ "Doesn't Add Up"),
           state = state_name)
  return(df_final)
}

#################
# Custom function to generate state data checklist
# If data is different from what was submitted before
#################

