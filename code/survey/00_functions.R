############################################
# Project:  MCLC Survey (2022)
# File: functions.R
# Last updated: October 19, 2022
# Author: Mari Roberts

# Custom functions to extract and format survey data
############################################

# Extract costs
fnc_extract_costs <- function(df, state_name){
  # Clean variable names
  # Extract cost data in spreadsheet
  df1 <- janitor::clean_names(df)
  df1 <- df1 %>% select(year_2019 = x6,
                             year_2020 = x7,
                             year_2021 = x8)
  df1 <- df1[46,]
  df1 <- df1 %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . ==  "n/a"           |
                                           . == "na"             |
                                           . ==  "nodata"        |
                                           . ==  "no data"       |
                                           . ==  "notavailable"  |
                                           . ==  "not available" |
                                           . ==  "nr"            |
                                           . ==  "notready"      |
                                           . ==  "not ready"     |
                                           . == "[none]"         |
                                           . == "none",           "No Data"))) %>%
    mutate(state = state_name)
}

# Extract data
fnc_extract_data <- function(df, state_name){
  # Clean variable names
  df1 <- janitor::clean_names(df)

  # Combine columns of text
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])

  # Select admissions and population data in spreadsheet
  # Rename variables
  # Remove white space and instructions that imported from Google Sheets
  df1 <- df1 %>% select(metric,
                        year_2018 = x5,
                        year_2019 = x6,
                        year_2020 = x7,
                        year_2021 = x8)
  df1 <- df1[c(22:31, 34:43),]

  # Indicate when data was left blank, NA was entered
  # If "null" then the respondent left the field blank
  # If "NA" (or variations of the spelling of NA) then the respondent inputed this and we label it as "No Data"
  df1 <- df1 %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . ==  "n/a"           |
                                            . == "na"             |
                                            . ==  "nodata"        |
                                            . ==  "no data"       |
                                            . ==  "notavailable"  |
                                            . ==  "not available" |
                                            . ==  "nr"            |
                                            . ==  "notready"      |
                                            . ==  "not ready"     |
                                            . == "[none]"         |
                                            . == "none",           "No Data"))) %>%

    mutate(year_2018 = ifelse(is.na(year_2018) | year_2018 == "null", "No Data",    year_2018),
           year_2019 = ifelse(is.na(year_2019) | year_2019 == "null", "No Data",    year_2019),
           year_2020 = ifelse(is.na(year_2020) | year_2020 == "null", "No Data",    year_2020)) %>%
    mutate(year_2021 = ifelse(is.na(year_2021) | year_2021 == "null", "Left Blank", year_2021)) %>%
    mutate(state = state_name)
}

