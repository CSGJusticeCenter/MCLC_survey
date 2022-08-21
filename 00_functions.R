############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: August 21, 2022
# Author: Mari Roberts

# Custom functions
############################################

#############################################
# Custom function to extract name and email
#############################################

# custom function to extract name and email from google sheets used in checklists.R
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
# NOTES AND COMMENTS
#############################################

# custom function to extract notes and additional comments used in checklists.R
fnc_notes_comments <- function(df, state_name){
  notes    <- df[49,2]
  comments <- df[49,10]
  df1 <- as.data.frame(c(notes, comments))
  df1 <- df1 %>%
    mutate(state = state_name) %>%
    rename(notes = 1,
           comments = 2)
}

#############################################
# COSTS
#############################################

# Custom function to extract costs used in checklists.R
fnc_costs <- function(df, state_name){
  # clean variable names
  df1 <- janitor::clean_names(df)

  # extract cost data in spreadsheet
  df_costs <- df1 %>% select(year_2019 = x6,
                             year_2020 = x7,
                             year_2021 = x8)
  df_costs <- df_costs[46,]
  df_costs <- as.data.frame(df_costs)

  # indicate when data was left blank, NA was entered
  # format numbers
  # a lot of code because one column can have a number and character data type
  # if "null" then the respondent left the field blank
  # if "NA" (or variations of the spelling of NA) then the respondent inputed this and we label it as "No Data"
  # add $ sign to dollar amounts
  df_costs <- df_costs %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . == "na" |
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
# DEFINITIONS CHECKLIST
#############################################

# custom function to extract definition confirmations used in checklists.R
fnc_definitions <- function(df, state_name){
  df1 <- janitor::clean_names(df)
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])
  df1 <- df1[c(22:31),]
  df1 <- df1 %>%
    select(metric, include = x19, dontinclude = x20, definition_confirmation = x14) %>%
    mutate(include = gsub("CONFIRMED - ", "", include),
           dontinclude = gsub("CONFIRMED - ", "", dontinclude)) %>%
    mutate(state = state_name,
           definition_confirmation = case_when(
           definition_confirmation == "TRUE"  ~ "Confirmed",
           definition_confirmation == "FALSE" ~ "Not Confirmed"
           )) %>%
    filter(definition_confirmation != "Confirmed")
}

#############################################
# STATE DATA CHECKLIST
#############################################

# custom function to generate state data checklist used in checklists.R
# if numbers don't add up, what was left blank, no data
fnc_create_state_data_checklist <- function(df, state_name){

  # clean variable names
  df1 <- janitor::clean_names(df)

  # combine columns of text
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])

  # select admissions and population data in spreadsheet
  # rename variables
  # remove white space and instructions that imported from Google Sheets
  df1 <- df1 %>% select(metric,
                        year_2018 = x5,
                        year_2019 = x6,
                        year_2020 = x7,
                        year_2021 = x8)
  df1 <- df1[c(22:31, 34:43),]

  # indicate when data was left blank, NA was entered
  # format numbers
  # a lot of code because one column can have a number and character data type
  # if "null" then the respondent left the field blank
  # if "NA" (or variations of the spelling of NA) then the respondent inputed this and we label it as "No Data"
  df1 <- df1 %>%
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
  df1 <- df1 %>%
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
  df_checks <- df1 %>%
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
  df1 <- df1 %>% select(-c(check_year_2018, check_year_2019, check_year_2020, check_year_2021))

  # transpose data
  df_transposed <- as.data.frame(t(df1))

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
  # new offense violations = new offense probation + new offense parole
  # technical violations = technical probation + technical parole
  df_final <- df_transposed %>%
    mutate(check_other_prison_admissions_22 = as.numeric(total_prison_admissions_22) - as.numeric(total_supervision_violation_admissions_22),

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

           check_other_prison_population_22 = as.numeric(total_prison_population_22) - as.numeric(total_supervision_violation_population_22),

           check_supervision_violation_population_22 = case_when(as.numeric(total_supervision_violation_population_22) == as.numeric(probation_violation_population_22) + as.numeric(parole_violation_population_22) ~ "Correct",
                                                                 as.numeric(total_supervision_violation_population_22) != as.numeric(probation_violation_population_22) + as.numeric(parole_violation_population_22) ~ "Doesn't Add Up",
                                                                 total_supervision_violation_population_22 == "No Data" ~ "No Data",
                                                                 total_supervision_violation_population_22 == "Left Blank" ~ "Left Blank",
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

           check_new_offense_violation_population_22 = case_when(as.numeric(total_new_offense_population_22) == as.numeric(new_offense_probation_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
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
           check_other_prison_population_22              = comma(check_other_prison_population_22, digits = 0))

  # identify NA vs Left Blank
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
# ADMISSIONS TABLE
#############################################

# custom function to generate an admissions table that shows survey 2021, survey 2022, and qa check "left blank"
fnc_adm_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_adm_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_21:new_offense_parole_violation_admissions_21)

  # filter data to state and select 2022 data
  df_adm_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_22:new_offense_parole_violation_admissions_22)

  # filter data to state and select data quality checks in previous survey checklist (where 2018-2022 numbers changed?)
  df_adm_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_admissions_21_22:check_new_offense_parole_violation_admissions_21_22)

  # reshape data so years are columns and metrics are rows
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

  # add data together and order metrics in table
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

#############################################
# POPULATION TABLE
#############################################

# population table
fnc_pop_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_pop_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_21:new_offense_parole_violation_population_21)

  # filter data to state and select 2022 data
  df_pop_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_22:new_offense_parole_violation_population_22)

  # filter data to state and select data quality checks in previous survey checklist (where 2018-2022 numbers changed?)
  df_pop_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_population_21_22:check_new_offense_parole_violation_population_21_22)

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

  # add data together and order metrics in table
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

# custom function to format table headers for admissions and population tables
fnc_headers <- function(gt_object){
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

# custom function to format table headers for costs table
fnc_headers_costs <- function(gt_object){
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

# custom function to format table for for all gt tables (spacing, font size, colors, etc.)
fnc_table_settings <- function(gt_object){
  gt_object %>%
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
    )
}

####################################################################
# gt table qa checks
# if data adds up
####################################################################

# QA table for state and metric with data quality issues - if data doesn't add up
fnc_gt_qa_table <- function(df, state_name, variable_1, variable_2, variable_3, variable_4, header){
  # filter by state and metrics selected
  df1 <- state_data_checklist %>%
    dplyr::filter(state == state_name) %>%
    dplyr::select(-c(state)) %>%
    dplyr::mutate(plus = "+",
                  equal = "=") %>%
    dplyr::select(year,
                  variable_1,
                  plus,
                  variable_2,
                  equal,
                  variable_3,
                  variable_4)

  # if the data doesn't add up, output a table, otherwise, leave blank
  test <- any(df1=="Doesn't Add Up")

  { if(test == TRUE){
    qa_table <- gt(df1) %>%

      # table title and subtitle
      tab_header(title = "Data may not be accurate") %>%
      tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
                locations = cells_title("title")) %>%

      # bold headers and year column
      tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(year))) %>%
      tab_style(locations = cells_column_labels(columns = everything()),
                style = list(cell_text(weight = "bold"))) %>%

      # add custom table settings and unique header (functions below) depending on metric
      fnc_table_settings() %>%
      header

  } else if(test == FALSE){
    qa_table <- ""
  }
  }
}

# table headers for QA supervision violation admissions
fnc_qa_supervision_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_supervision_violation_admissions_22" ~ px(80),
      "probation_violation_admissions_22" ~ px(80),
      "parole_violation_admissions_22" ~ px(80),
      "check_supervision_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_supervision_violation_admissions_22  = "Total Supervision Violation admissions",
      probation_violation_admissions_22          = "Probation Violation admissions",
      parole_violation_admissions_22             = "Parole Violation admissions",
      check_supervision_violation_admissions_22  = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA probation admissions
fnc_qa_probation_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "probation_violation_admissions_22" ~ px(80),
      "new_offense_probation_violation_admissions_22" ~ px(80),
      "technical_probation_violation_admissions_22" ~ px(80),
      "check_probation_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      probation_violation_admissions_22             = "Probation Violation Admissions",
      new_offense_probation_violation_admissions_22 = "New Offense Probation Violation Admissions",
      technical_probation_violation_admissions_22   = "Technical Probation Violation Admissions",
      check_probation_violation_admissions_22       = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA parole admissions
fnc_qa_parole_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "parole_violation_admissions_22" ~ px(80),
      "new_offense_parole_violation_admissions_22" ~ px(80),
      "technical_parole_violation_admissions_22" ~ px(80),
      "check_parole_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      parole_violation_admissions_22             = "Parole Violation admissions",
      new_offense_parole_violation_admissions_22 = "New Offense Parole Violation admissions",
      technical_parole_violation_admissions_22   = "Technical Parole Violation admissions",
      check_parole_violation_admissions_22       = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA technical violation admissions
fnc_qa_technical_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_technical_violation_admissions_22" ~ px(80),
      "technical_probation_violation_admissions_22" ~ px(80),
      "technical_parole_violation_admissions_22" ~ px(80),
      "check_total_technical_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_technical_violation_admissions_22       = "Total Technical Violation Admissions",
      technical_probation_violation_admissions_22   = "Technical Probation Violation Admissions",
      technical_parole_violation_admissions_22      = "Technical Parole Violation Admissions",
      check_total_technical_violation_admissions_22 = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA new offense violation admissions
fnc_qa_new_offense_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_new_offense_admissions_22" ~ px(80),
      "new_offense_probation_violation_admissions_22" ~ px(80),
      "new_offense_parole_violation_admissions_22" ~ px(80),
      "check_new_offense_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_new_offense_admissions_22                 = "Total New Offense Violation admissions",
      new_offense_probation_violation_admissions_22   = "New Offense Probation Violation admissions",
      new_offense_parole_violation_admissions_22      = "New Offense Parole Violation admissions",
      check_new_offense_violation_admissions_22       = "Data Quality Check",
      plus                                            = " ",
      equal                                           = " ")
}

# table headers for QA supervision violation population
fnc_qa_supervision_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_supervision_violation_population_22" ~ px(80),
      "probation_violation_population_22" ~ px(80),
      "parole_violation_population_22" ~ px(80),
      "check_supervision_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_supervision_violation_population_22  = "Total Supervision Violation Population",
      probation_violation_population_22          = "Probation Violation Population",
      parole_violation_population_22             = "Parole Violation Population",
      check_supervision_violation_population_22  = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA probation population
fnc_qa_probation_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "probation_violation_population_22" ~ px(80),
      "new_offense_probation_violation_population_22" ~ px(80),
      "technical_probation_violation_population_22" ~ px(80),
      "check_probation_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      probation_violation_population_22             = "Probation Violation Population",
      new_offense_probation_violation_population_22 = "New Offense Probation Violation Population",
      technical_probation_violation_population_22   = "Technical Probation Violation Population",
      check_probation_violation_population_22       = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA parole population
fnc_qa_parole_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "parole_violation_population_22" ~ px(80),
      "new_offense_parole_violation_population_22" ~ px(80),
      "technical_parole_violation_population_22" ~ px(80),
      "check_parole_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      parole_violation_population_22             = "Parole Violation Population",
      new_offense_parole_violation_population_22 = "New Offense Parole Violation Population",
      technical_parole_violation_population_22   = "Technical Parole Violation Population",
      check_parole_violation_population_22       = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA technical violation population
fnc_qa_technical_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_technical_violation_population_22" ~ px(80),
      "technical_probation_violation_population_22" ~ px(80),
      "technical_parole_violation_population_22" ~ px(80),
      "check_total_technical_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_technical_violation_population_22       = "Total Technical Violation Population",
      technical_probation_violation_population_22   = "Technical Probation Violation Population",
      technical_parole_violation_population_22      = "Technical Parole Violation Population",
      check_total_technical_violation_population_22 = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA new offense violation population
fnc_qa_new_offense_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_new_offense_population_22" ~ px(80),
      "new_offense_probation_violation_population_22" ~ px(80),
      "new_offense_parole_violation_population_22" ~ px(80),
      "check_new_offense_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_new_offense_population_22                 = "Total New Offense Violation Population",
      new_offense_probation_violation_population_22   = "New Offense Probation Violation Population",
      new_offense_parole_violation_population_22      = "New Offense Parole Violation Population",
      check_new_offense_violation_population_22       = "Data Quality Check",
      plus                                            = " ",
      equal                                           = " ")
}

#####################################################################################
# gt table for definition confirmations
#####################################################################################

fnc_gt_definitions_table <- function(df, state_name){

  # filter by state and metrics selected
  df1 <- definitions_table_checklist %>%
    dplyr::filter(state == state_name) %>%
    dplyr::select(-c(state))

  definitions_table <- gt(df1) %>%

    # table title and subtitle
    tab_header(title = "Definitions") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%

    # Set missing value defaults
    fmt_missing(columns = gt::everything(), missing_text = "") %>%

    # add custom table settings and unique header (functions below) depending on metric
    fnc_table_settings() %>%

    cols_width(
      "metric" ~ px(250),
      "include" ~ px(400),
      "dontinclude" ~ px(200),
      "definition_confirmation" ~  px(100)) %>%
    cols_label(
      metric = "Data",
      include = "Definitions",
      dontinclude = " ",
      definition_confirmation = "Confirm Definition") %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = definition_confirmation, rows = definition_confirmation == "Not Confirmed")) %>%

    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = definition_confirmation, rows = definition_confirmation != "Not Confirmed")) %>%

    # change to raw html for email
    as_raw_html()

  return(definitions_table)
}


#####################################################################################
# gt table for admissions
#####################################################################################

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
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

 return(adm_table)
}

####################################################################
# gt table for population
####################################################################

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
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(pop_table)
}

####################################################################
# gt table for costs
####################################################################

fnc_gt_costs_table <- function(df, state_name){

  # filter by state
  df <- costs_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  costs_table <- gt(df) %>%

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
              locations = cells_body(columns = c(current_2019))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers_costs() %>%

    # change color to yellow if a field was left blank
    # change colors to green if data was changed between years
    # change colors to green if the data is new (2021)
    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = current_2020 != "Left Blank" & previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(costs_table)
}

####################################################################
# gt table for notes and comments
####################################################################

fnc_gt_notes_comments_table <- function(df, state_name){

  # filter by state
  df <- notes_comments_list %>%
    clean_names() %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  notes_comments_table <- gt(df) %>%

  # table title and subtitle
  tab_header(title = "Notes and Comments") %>%
  tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
            locations = cells_title("title")) %>%

  # border lines around 2021 survey
  tab_style(style = list(cell_borders(side = c("right"), color = "gray", weight = px(1))),
            locations = cells_body(columns = c(notes))) %>%

  # appearance settings
  fnc_table_settings() %>%

  cols_width(
    "notes" ~ px(480),
    "comments" ~ px(280)) %>%
  cols_label(
    notes = "State Notes",
    comments = "Additional Comments") %>%

  # change to raw html for email
  as_raw_html()

  return(notes_comments_table)

}
