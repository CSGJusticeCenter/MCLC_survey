############################################
# Project:  MCLC Survey (2022)
# File: libary_functions.R
# Last updated: February 21, 2023 (MAR)
# Author: Mari Roberts

# Load packages
# Load custom functions
############################################

# Instructions for installing the csgjcr package
# In your Renviron (usethis::edit_r_environ(), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"

# remove current csgjcr package and download the develop branch to be able to use
#     the function csg_set_project_path
# remove.packages("csgjcr")
# devtools::install_github("CSGJusticeCenter/csgjcr@develop")

# Set project path for MAR/JM - un-comment your part
# csg_set_project_path(project = "MCLC",
#                      sp_folder = "C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project", force = TRUE)

csg_set_project_path(project = "MCLC",
                     sp_folder = "C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project", force = TRUE)

# assign SP path
sp_data_path <- csg_get_project_path("MCLC")

#####
# Load packages
#####

library(csgjcr)
library(googlesheets4)
library(googledrive)
library(readxl)
library(janitor)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(formattable)
library(data.table)
library(blastula)
library(Microsoft365R)
library(webshot)
library(shiny)
library(tidyverse)
library(reactable)
library(glue)
library(gt)
library(gtExtras)
library(openxlsx)
library(eeptools)
library(Hmisc)
library(svDialogs)
library(readr)
library(reshape)
library(mice)
library(VIM)
library(finalfit)
library(scales)
library(xtable)

#####
# Custom functions
#####

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
    mutate(across(everything(), ~replace(., . ==  "n/a"          |
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
