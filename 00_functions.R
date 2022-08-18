############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: August 18, 2022
# Author: Mari Roberts

# Custom functions
############################################

#############################################
# Custom function to extract name and email
#############################################

# custom function to extract name and email
fnc_contact_info <- function(df, state_name){
  name  <- df[18,2]
  email <- df[18,4]
  df1 <- as.data.frame(c(name, email))
  df1 <- df1 %>%
    mutate(state_name = state_name) %>%
    rename(name = 1,
           email = 2)
}

# make string title case
clean_titles <- function(x) {
  x <- gsub("_", " ", x)
  x <- gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
  x
}

#############################################
# Custom function to extract notes and additional comments
#############################################

# custom function to extract notes and additional comments
fnc_notes_comments <- function(df, state_name){
  notes    <- df[49,2]
  comments <- df[49,10]
  df1 <- as.data.frame(c(notes, comments))
  df1 <- df1 %>%
    mutate(state_name = state_name) %>%
    rename(notes = 1,
           comments = 2)
}

#############################################
# Custom function to extract costs
#############################################

fnc_costs <- function(df, state_name){
  # clean variable names
  df1 <- janitor::clean_names(df)

  # cost data in spreadsheet
  df_costs <- df1 %>% select(year_2019 = x6,
                             year_2020 = x7,
                             year_2021 = x8)
  df_costs <- df_costs[46,]
  df_costs <- as.data.frame(df_costs)

  # indicate when data was left blank, NA was entered
  # format numbers to currency
  # a lot of code because one column can have number and character data type
  df_costs <- df_costs %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . ==  "na" |
                                           . ==  "nodata" |
                                           . ==  "no data" |
                                           . ==  "notavailable" |
                                           . ==  "not available" |
                                           . ==  "notready" |
                                           . ==  "not ready" |
                                           . == "[none]" |
                                           . == "none"
                                         , "No Data"))) %>%
    mutate(across(everything(), ~replace(., . ==  "null", "Left Blank"))) %>%
    mutate(check_year_2019 = case_when(year_2019 == "No Data" ~ "No Data",
                                       year_2019 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2020 = case_when(year_2020 == "No Data" ~ "No Data",
                                       year_2020 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2021 = case_when(year_2021 == "No Data" ~ "No Data",
                                       year_2021 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete")) %>%
    mutate(across(c(year_2019, year_2020, year_2021), as.numeric)) %>%
    mutate_if(is.numeric,funs(comma(., digits = 2))) %>%
    mutate_if(is.numeric,funs(paste0("$", .))) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(state = state_name) %>%
    mutate(year_2019 = case_when(year_2019 == "$ NA" ~ check_year_2019,
                                 TRUE ~ year_2019),
           year_2020 = case_when(year_2020 == "$ NA" ~ check_year_2020,
                                 TRUE ~ year_2020),
           year_2021 = case_when(year_2021 == "$ NA" ~ check_year_2021,
                                 TRUE ~ year_2021)) %>%
    select(-c(check_year_2019, check_year_2020, check_year_2021))
}

#############################################
# Custom function to generate state data checklist
# If numbers don't add up, what was left blank, no data
#############################################

fnc_create_state_data_checklist <- function(df, state_name){

  # clean variable names
  df1 <- janitor::clean_names(df)

  # combine columns of text
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])

  # select admissions and population data in spreadsheet
  # rename variables
  # remove white space and instructions that imported from Google Sheets
  # add commas to numbers
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

  # need to add commas to numbers but the column is character because of Left Blank and No Data
  # which is info we want
  # create temporary columns that capture this info
  df_numbers <- df_numbers %>%
    mutate(check_year_2018 = case_when(year_2018 == "No Data" ~ "No Data",
                                       year_2018 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2019 = case_when(year_2019 == "No Data" ~ "No Data",
                                       year_2019 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2020 = case_when(year_2020 == "No Data" ~ "No Data",
                                       year_2020 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2021 = case_when(year_2021 == "No Data" ~ "No Data",
                                       year_2021 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"))

  # identify which NAs were "No Data" or "Left Blank"
  df_checks <- df_numbers %>%
    mutate(across(everything(), as.character)) %>%
    mutate(year_2018 = case_when(check_year_2018 == "No Data" ~ "No Data",
                                 check_year_2018 == "Left Blank" ~ "Left Blank",
                                 check_year_2018 == "Complete" ~ year_2018),
           year_2019 = case_when(check_year_2019 == "No Data" ~ "No Data",
                                 check_year_2019 == "Left Blank" ~ "Left Blank",
                                 check_year_2019 == "Complete" ~ year_2019),
           year_2020 = case_when(check_year_2020 == "No Data" ~ "No Data",
                                 check_year_2020 == "Left Blank" ~ "Left Blank",
                                 check_year_2020 == "Complete" ~ year_2020),
           year_2021 = case_when(check_year_2021 == "No Data" ~ "No Data",
                                 check_year_2021 == "Left Blank" ~ "Left Blank",
                                 check_year_2021 == "Complete" ~ year_2021)) %>%
    select(c(metric, check_year_2018, check_year_2019, check_year_2020, check_year_2021))

  # select variables
  df_numbers <- df_numbers %>% select(-c(check_year_2018, check_year_2019, check_year_2020, check_year_2021))

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
    rename_with(~ paste0(., "_22"), -c(year)) %>%
    mutate(across(everything(), as.numeric))

  # check to see if numbers add up correctly
  # supervision violations = probation + parole violations
  # probation violations = technical probation + new offense probation
  # parole violations = technical parole + new offense parole
  df_final <- df_transposed %>%
    mutate(check_other_prison_admissions_22 = as.numeric(total_prison_admissions_22) - as.numeric(total_supervision_violation_admissions_22),
           check_other_prison_population_22 = as.numeric(total_prison_population_22) - as.numeric(total_supervision_violation_population_22),

           check_supervision_violation_admissions_22 = case_when(as.numeric(total_supervision_violation_admissions_22) == as.numeric(probation_violation_admissions_22) + as.numeric(parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(total_supervision_violation_admissions_22) != as.numeric(probation_violation_admissions_22) + as.numeric(parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 total_supervision_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 total_supervision_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),
           check_probation_violation_admissions_22   = case_when(as.numeric(probation_violation_admissions_22) == as.numeric(technical_probation_violation_admissions_22) + as.numeric(new_offense_probation_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(probation_violation_admissions_22) != as.numeric(technical_probation_violation_admissions_22) + as.numeric(new_offense_probation_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 probation_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 probation_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),
           check_parole_violation_admissions_22      = case_when(as.numeric(parole_violation_admissions_22) == as.numeric(technical_parole_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(parole_violation_admissions_22) != as.numeric(technical_parole_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 parole_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 parole_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),
           check_probation_violation_population_22   = case_when(as.numeric(probation_violation_population_22) == as.numeric(technical_probation_violation_population_22) + as.numeric(new_offense_probation_violation_population_22) ~ "Correct",
                                                                 as.numeric(probation_violation_population_22) != as.numeric(technical_probation_violation_population_22) + as.numeric(new_offense_probation_violation_population_22) ~ "Doesn't Add Up",
                                                                 probation_violation_population_22 == "No Data" ~ "No Data",
                                                                 probation_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),
           check_parole_violation_population_22      = case_when(as.numeric(parole_violation_population_22) == as.numeric(technical_parole_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
                                                                 as.numeric(parole_violation_population_22) != as.numeric(technical_parole_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                 parole_violation_population_22 == "No Data" ~ "No Data",
                                                                 parole_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_new_offense_violation_admissions_22 = case_when(as.numeric(total_new_offense_admissions_22) == as.numeric(new_offense_probation_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(total_new_offense_admissions_22) != as.numeric(new_offense_probation_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 total_new_offense_admissions_22 == "No Data" ~ "No Data",
                                                                 total_new_offense_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_total_technical_violation_admissions_22 = case_when(as.numeric(total_technical_violation_admissions_22) == as.numeric(technical_probation_violation_admissions_22) + as.numeric(technical_parole_violation_admissions_22) ~ "Correct",
                                                                     as.numeric(total_technical_violation_admissions_22) != as.numeric(technical_probation_violation_admissions_22) + as.numeric(technical_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                     total_technical_violation_admissions_22 == "No Data" ~ "No Data",
                                                                     total_technical_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                     TRUE ~ "No Data"),

           check_new_offense_violation_population_22     = case_when(as.numeric(total_new_offense_population_22) == as.numeric(new_offense_probation_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
                                                                     as.numeric(total_new_offense_population_22) != as.numeric(new_offense_probation_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                     total_new_offense_population_22 == "No Data" ~ "No Data",
                                                                     total_new_offense_population_22 == "Left Blank" ~ "Left Blank",
                                                                     TRUE ~ "No Data"),

           check_total_technical_violation_population_22 = case_when(as.numeric(total_technical_violation_population_22) == as.numeric(technical_probation_violation_population_22) + as.numeric(technical_parole_violation_population_22) ~ "Correct",
                                                                     as.numeric(total_technical_violation_population_22) != as.numeric(technical_probation_violation_population_22) + as.numeric(technical_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                     total_technical_violation_population_22 == "No Data" ~ "No Data",
                                                                     total_technical_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                     TRUE ~ "No Data"),

           state = state_name)

  # transpose data
  df_transposed_checks <- as.data.frame(t(df_checks))

  # make first row header and get year
  df_transposed_checks <- df_transposed_checks %>%
    row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    tibble::rownames_to_column("year") %>%
    mutate(year = case_when(grepl("2018", year) ~ 2018,
                            grepl("2019", year) ~ 2019,
                            grepl("2020", year) ~ 2020,
                            grepl("2021", year) ~ 2021)) %>%
    rename_with(~ paste0("entry_check_", . , "_22"), -c(year))

  # merge with final data
  df_final <- merge(df_final, df_transposed_checks, by = "year")

  # identify NA vs Left Blank
  # add commas to numbers
  df_final <- df_final %>%
    mutate(total_prison_admissions_22                    = comma(total_prison_admissions_22, digits = 0),
           total_supervision_violation_admissions_22     = comma(total_supervision_violation_admissions_22, digits = 0),
           probation_violation_admissions_22             = comma(probation_violation_admissions_22, digits = 0),
           parole_violation_admissions_22                = comma(parole_violation_admissions_22, digits = 0),
           total_technical_violation_admissions_22       = comma(total_technical_violation_admissions_22, digits = 0),
           technical_probation_violation_admissions_22   = comma(technical_probation_violation_admissions_22, digits = 0),
           technical_parole_violation_admissions_22      = comma(technical_parole_violation_admissions_22, digits = 0),
           total_new_offense_admissions_22               = comma(total_new_offense_admissions_22, digits = 0),
           new_offense_probation_violation_admissions_22 = comma(new_offense_probation_violation_admissions_22, digits = 0),
           new_offense_parole_violation_admissions_22    = comma(new_offense_parole_violation_admissions_22, digits = 0),

           total_prison_population_22                    = comma(total_prison_population_22, digits = 0),
           total_supervision_violation_population_22     = comma(total_supervision_violation_population_22, digits = 0),
           probation_violation_population_22             = comma(probation_violation_population_22, digits = 0),
           parole_violation_population_22                = comma(parole_violation_population_22, digits = 0),
           total_technical_violation_population_22       = comma(total_technical_violation_population_22, digits = 0),
           technical_probation_violation_population_22   = comma(technical_probation_violation_population_22, digits = 0),
           technical_parole_violation_population_22      = comma(technical_parole_violation_population_22, digits = 0),
           total_new_offense_population_22               = comma(total_new_offense_population_22, digits = 0),
           new_offense_probation_violation_population_22 = comma(new_offense_probation_violation_population_22, digits = 0),
           new_offense_parole_violation_population_22    = comma(new_offense_parole_violation_population_22, digits = 0),

           check_other_prison_admissions_22              = comma(check_other_prison_admissions_22, digits = 0),
           check_other_prison_population_22              = comma(check_other_prison_population_22, digits = 0)
           )

  df_final <- df_final %>%
    mutate(across(everything(), as.character))  %>%

    mutate(total_prison_admissions_22                     = case_when(total_prison_admissions_22 == "NA"  ~ entry_check_total_prison_admissions_22,
                                                                      TRUE ~ total_prison_admissions_22),
           total_supervision_violation_admissions_22      = case_when(total_supervision_violation_admissions_22 == "NA"  ~ entry_check_total_supervision_violation_admissions_22,
                                                                      TRUE ~ total_supervision_violation_admissions_22),
           probation_violation_admissions_22              = case_when(probation_violation_admissions_22 == "NA"  ~ entry_check_probation_violation_admissions_22,
                                                                      TRUE ~ probation_violation_admissions_22),
           parole_violation_admissions_22                 = case_when(parole_violation_admissions_22 == "NA"  ~ entry_check_parole_violation_admissions_22,
                                                                      TRUE ~ parole_violation_admissions_22),
           total_technical_violation_admissions_22        = case_when(total_technical_violation_admissions_22 == "NA"  ~ entry_check_total_technical_violation_admissions_22,
                                                                      TRUE ~ total_technical_violation_admissions_22),
           technical_probation_violation_admissions_22    = case_when(technical_probation_violation_admissions_22 == "NA"  ~ entry_check_technical_probation_violation_admissions_22,
                                                                      TRUE ~ technical_probation_violation_admissions_22),
           technical_parole_violation_admissions_22       = case_when(technical_parole_violation_admissions_22 == "NA"  ~ entry_check_technical_parole_violation_admissions_22,
                                                                      TRUE ~ technical_parole_violation_admissions_22),
           total_new_offense_population_22                = case_when(total_new_offense_population_22 == "NA"  ~ entry_check_total_new_offense_population_22,
                                                                      TRUE ~ total_new_offense_population_22),
           new_offense_probation_violation_population_22  = case_when(new_offense_probation_violation_population_22 == "NA"  ~ entry_check_new_offense_probation_violation_population_22,
                                                                      TRUE ~ new_offense_probation_violation_population_22),
           new_offense_parole_violation_population_22     = case_when(new_offense_parole_violation_population_22 == "NA"  ~ entry_check_new_offense_parole_violation_population_22,
                                                                      TRUE ~ new_offense_parole_violation_population_22),
           total_prison_population_22                     = case_when(total_prison_population_22 == "NA"  ~ entry_check_total_prison_population_22,
                                                                      TRUE ~ total_prison_population_22),
           total_supervision_violation_population_22      = case_when(total_supervision_violation_population_22 == "NA"  ~ entry_check_total_supervision_violation_population_22,
                                                                      TRUE ~ total_supervision_violation_population_22),
           probation_violation_population_22              = case_when(probation_violation_population_22 == "NA"  ~ entry_check_probation_violation_population_22,
                                                                      TRUE ~ probation_violation_population_22),
           parole_violation_population_22                 = case_when(parole_violation_population_22 == "NA"  ~ entry_check_parole_violation_population_22,
                                                                      TRUE ~ parole_violation_population_22),
           total_technical_violation_population_22        = case_when(total_technical_violation_population_22 == "NA"  ~ entry_check_total_technical_violation_population_22,
                                                                      TRUE ~ total_technical_violation_population_22),
           technical_probation_violation_population_22    = case_when(technical_probation_violation_population_22 == "NA"  ~ entry_check_technical_probation_violation_population_22,
                                                                      TRUE ~ technical_probation_violation_population_22),
           technical_parole_violation_population_22       = case_when(technical_parole_violation_population_22 == "NA"  ~ entry_check_technical_parole_violation_population_22,
                                                                      TRUE ~ technical_parole_violation_population_22),
           total_new_offense_population_22                = case_when(total_new_offense_population_22 == "NA"  ~ entry_check_total_new_offense_population_22,
                                                                      TRUE ~ total_new_offense_population_22),
           new_offense_probation_violation_population_22  = case_when(new_offense_probation_violation_population_22 == "NA"  ~ entry_check_new_offense_probation_violation_population_22,
                                                                      TRUE ~ new_offense_probation_violation_population_22),
           new_offense_parole_violation_population_22     = case_when(new_offense_parole_violation_population_22 == "NA"  ~ entry_check_new_offense_parole_violation_population_22,
                                                                      TRUE ~ new_offense_parole_violation_population_22)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), "No Data", .)))
  return(df_final)
}

#############################################
# Custom function to generate previous survey checklist
# If data is different from what was submitted before
#############################################

# admissions table
fnc_adm_table_checklist <- function(df, state_name){

  ##############

  # filter data to state and select 2021 data
  df_adm_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_21:new_offense_parole_violation_admissions_21)

  # filter data to state and select 2022 data
  df_adm_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_22:new_offense_parole_violation_admissions_22)

  # filter data to state and select data quality checks in previous survey checklist
  df_adm_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_admissions_21_22:check_new_offense_parole_violation_admissions_21_22)

  ##############

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
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

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
  df_adm_check_21_22 <- reshape2::dcast(reshape2::melt(df_adm_check_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_check_21_22 <- df_adm_check_21_22 %>%
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

  ##############

  # add data together and order metrics
  # change data types
  df_adm <- merge(df_adm_21, df_adm_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_adm <- merge(df_adm, df_adm_check_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
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
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}

# population table
fnc_pop_table_checklist <- function(df, state_name){

  ##############

  # filter data to state and select 2021 data
  df_pop_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_21:new_offense_parole_violation_population_21)

  # filter data to state and select 2022 data
  df_pop_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_22:new_offense_parole_violation_population_22)

  # filter data to state and select data quality checks in previous survey checklist
  df_pop_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_population_21_22:check_new_offense_parole_violation_population_21_22)

  ##############

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
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

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
  df_pop_check_21_22 <- reshape2::dcast(reshape2::melt(df_pop_check_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_check_21_22 <- df_pop_check_21_22 %>%
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

  ##############

  # add data together and order metrics
  # change data types
  df_pop <- merge(df_pop_21, df_pop_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_pop <- merge(df_pop, df_pop_check_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
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
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}

#############################################
# GT Tables for Email
#############################################

# table customizations for admissions and population tables
fnc_table_customizations <- function(gt_object){
  gt_object %>%
    cols_width(
      "metric"        ~ px(270),
      "previous_2018" ~ px(70),
      "previous_2019" ~ px(70),
      "previous_2020" ~ px(70),
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

# table customizations for costs table
fnc_table_customizations_costs <- function(gt_object){
  gt_object %>%
    cols_width(
      "previous_2019" ~ px(70),
      "previous_2020" ~ px(70),
      "current_2019"  ~ px(70),
      "current_2020"  ~ px(70),
      "current_2021"  ~ px(70)
    ) %>%
    cols_label(
      previous_2019 = "2019",
      previous_2020 = "2020",
      current_2019	= "2019",
      current_2020	= "2020",
      current_2021	= "2021"
    )
}

# gt table for admissions
fnc_gt_adm_table <- function(df, state_name){

  # filter by state
  df <- adm_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  adm_table <- gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Prison Admissions", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(previous_2018))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2018))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2018_21_22, check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    tab_options(#table.width = px(760),
      table.align = "left",
      heading.align = "left",

      # remove row at top
      table.border.top.style = "hidden",
      # table.border.bottom.style = "transparent",
      heading.border.bottom.style = "hidden",
      table.border.bottom.color = "gray",

      # need to set this to transparent so that cells_borders of the cells can display properly
      table_body.border.bottom.style = "transparent",
      table_body.border.top.style = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "gray",

      # font sizes
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(12),
      column_labels.font.size = px(12),
      table.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.font.size = px(12),

      # row group label and border options
      row_group.font.size = px(12),
      row_group.border.top.style = "transparent",
      row_group.border.bottom.style = "hidden",
      stub.border.style = "dashed"
    ) %>%

    # specifications for column widths and labels
    fnc_table_customizations() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2018, current_2018), rows = previous_2018 != current_2018)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2019, current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2020, current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

 return(adm_table)
}

# gt table for population
fnc_gt_pop_table <- function(df, state_name){

  # filter by state
  df <- pop_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  pop_table <- gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Prison Population", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(previous_2018))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2018))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2018_21_22, check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    tab_options(#table.width = px(760),
      table.align = "left",
      heading.align = "left",

      # remove row at top
      table.border.top.style = "hidden",
      # table.border.bottom.style = "transparent",
      heading.border.bottom.style = "hidden",
      table.border.bottom.color = "gray",

      # need to set this to transparent so that cells_borders of the cells can display properly
      table_body.border.bottom.style = "transparent",
      table_body.border.top.style = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "gray",

      # font sizes
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(12),
      column_labels.font.size = px(12),
      table.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.font.size = px(12),

      # row group label and border options
      row_group.font.size = px(12),
      row_group.border.top.style = "transparent",
      row_group.border.bottom.style = "hidden",
      stub.border.style = "dashed"
    ) %>%

    # specifications for column widths and labels
    fnc_table_customizations() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2018, current_2018), rows = previous_2018 != current_2018)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2019, current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2020, current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(pop_table)
}

fnc_gt_costs_table <- function(df, state_name){

  # filter by state
  df <- costs_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Cost Per Day Per Person", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(previous_2019))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2019))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    tab_options(#table.width = px(760),
      table.align = "left",
      heading.align = "left",

      # remove row at top
      table.border.top.style = "hidden",
      # table.border.bottom.style = "transparent",
      heading.border.bottom.style = "hidden",
      table.border.bottom.color = "gray",

      # need to set this to transparent so that cells_borders of the cells can display properly
      table_body.border.bottom.style = "transparent",
      table_body.border.top.style = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "gray",

      # font sizes
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(12),
      column_labels.font.size = px(12),
      table.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.font.size = px(12),

      # row group label and border options
      row_group.font.size = px(12),
      row_group.border.top.style = "transparent",
      row_group.border.bottom.style = "hidden",
      stub.border.style = "dashed"
    ) %>%

    # specifications for column widths and labels
    fnc_table_customizations_costs() %>%

    # change color to yellow if a field was left blank
    # change colors to green if data was changed between years
    # change colors to green if the data is new (2021)
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2019, current_2019), rows = current_2019 != "Left Blank" &
                                       previous_2019 != current_2019)) %>%

    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2020, current_2020), rows = current_2020 != "Left Blank" &
                                       previous_2020 != current_2020)) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank"))
}
