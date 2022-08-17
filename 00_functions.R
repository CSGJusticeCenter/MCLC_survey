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
# Custom function to generate previous survey checklist
# If data is different from what was submitted before
#################

# admissions table
fnc_adm_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_adm_21 <- previous_survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_21:new_offense_parole_violation_admissions_21) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank or No Data", .)))

  # filter data to state and select data quality checks in previous survey checklist
  df_adm_21_22 <- previous_survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_admissions_21_22:check_new_offense_parole_violation_admissions_21_22)

  # filter data to state om state data checklist
  df_adm_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_22:new_offense_parole_violation_admissions_22)

  # reshape data
  df_adm_22 <- reshape2::dcast(reshape2::melt(df_adm_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_22 <- df_adm_22 %>%
    clean_names() %>%
    rename(current_2018 = x2018,
           current_2019 = x2019,
           current_2020 = x2020,
           current_2021 = x2021,
           metric = variable) %>%
    mutate(metric = gsub("_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_adm_21 <- reshape2::dcast(reshape2::melt(df_adm_21, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_21 <- df_adm_21 %>%
    clean_names() %>%
    rename(previous_2018 = x2018,
           previous_2019 = x2019,
           previous_2020 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(previous_2018 = comma(previous_2018, digits = 0),
           previous_2019 = comma(previous_2019, digits = 0),
           previous_2020 = comma(previous_2020, digits = 0)) %>%
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_adm_21_22 <- reshape2::dcast(reshape2::melt(df_adm_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_21_22 <- df_adm_21_22 %>%
    clean_names() %>%
    rename(check_2018_21_22 = x2018,
           check_2019_21_22 = x2019,
           check_2020_21_22 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # add data together and order metrics
  # change data types
  df_adm <- merge(df_adm_21, df_adm_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_adm <- merge(df_adm, df_adm_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_adm <- df_adm %>%
    mutate(order = case_when(
      metric == "Total Prison Admissions"                     ~ 1,
      metric == "Total Supervision Violation Admissions"      ~ 2,
      metric == "Probation Violation Admissions"              ~ 3,
      metric == "Parole Violation Admissions"                 ~ 4,
      metric == "Total Technical Violation Admissions"        ~ 5,
      metric == "Technical Probation Violation Admissions"    ~ 6,
      metric == "Technical Parole Violation Admissions"       ~ 7,
      metric == "Total New Offense Admissions"                ~ 8,
      metric == "New Offense Probation Violation Admissions"  ~ 9,
      metric == "New Offense Parole Violation Admissions"     ~ 10
    ),
    current_2018 = as.numeric(current_2018),
    current_2019 = as.numeric(current_2019),
    current_2020 = as.numeric(current_2020),
    current_2021 = as.numeric(current_2021),
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}

# population table
fnc_pop_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_pop_21 <- previous_survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_21:new_offense_parole_violation_population_21) %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank or No Data", .)))

  # filter data to state and select data quality checks in previous survey checklist
  df_pop_21_22 <- previous_survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_population_21_22:check_new_offense_parole_violation_population_21_22)

  # filter data to state om state data checklist
  df_pop_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_22:new_offense_parole_violation_population_22)

  # reshape data
  df_pop_22 <- reshape2::dcast(reshape2::melt(df_pop_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_22 <- df_pop_22 %>%
    clean_names() %>%
    rename(current_2018 = x2018,
           current_2019 = x2019,
           current_2020 = x2020,
           current_2021 = x2021,
           metric = variable) %>%
    mutate(metric = gsub("_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_pop_21 <- reshape2::dcast(reshape2::melt(df_pop_21, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_21 <- df_pop_21 %>%
    clean_names() %>%
    rename(previous_2018 = x2018,
           previous_2019 = x2019,
           previous_2020 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(previous_2018 = comma(previous_2018, digits = 0),
           previous_2019 = comma(previous_2019, digits = 0),
           previous_2020 = comma(previous_2020, digits = 0)) %>%
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_pop_21_22 <- reshape2::dcast(reshape2::melt(df_pop_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_21_22 <- df_pop_21_22 %>%
    clean_names() %>%
    rename(check_2018_21_22 = x2018,
           check_2019_21_22 = x2019,
           check_2020_21_22 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # add data together and order metrics
  # change data types
  df_pop <- merge(df_pop_21, df_pop_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_pop <- merge(df_pop, df_pop_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_pop <- df_pop %>%
    mutate(order = case_when(
      metric == "Total Prison Population"                     ~ 1,
      metric == "Total Supervision Violation Population"      ~ 2,
      metric == "Probation Violation Population"              ~ 3,
      metric == "Parole Violation Population"                 ~ 4,
      metric == "Total Technical Violation Population"        ~ 5,
      metric == "Technical Probation Violation Population"    ~ 6,
      metric == "Technical Parole Violation Population"       ~ 7,
      metric == "Total New Offense Population"                ~ 8,
      metric == "New Offense Probation Violation Population"  ~ 9,
      metric == "New Offense Parole Violation Population"     ~ 10
    ),
    current_2018 = as.numeric(current_2018),
    current_2019 = as.numeric(current_2019),
    current_2020 = as.numeric(current_2020),
    current_2021 = as.numeric(current_2021),
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}

#####

fnc_add_specific_customizations <- function(gt_object){
  gt_object %>%
    cols_width(
      "metric"        ~ px(270),
      "previous_2018" ~ px(70),
      "previous_2018" ~ px(70),
      "previous_2018" ~ px(70),
      "current_2018"  ~ px(70),
      "current_2019"  ~ px(70),
      "current_2020"  ~ px(70),
      "current_2021"  ~ px(70)
    ) %>%
    cols_label(
      metric        = "Data",
      previous_2018 = "2018",
      previous_2019 = "2019",
      previous_2020 = "2020",
      current_2018	= "2018",
      current_2019	= "2019",
      current_2020	= "2020",
      current_2021	= "2021"
    )
}
