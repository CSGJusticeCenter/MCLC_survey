############################################
# Project:  MCLC Survey (2022)
# File: library_functions.R
# Last updated: September 7, 2023

# Load packages
# Load custom functions
############################################

###################    Attention    #####################

# Instructions for installing the csgjcr package
# In your Renviron (usethis::edit_r_environ(),
# set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"
# remotes::install_github("csgjusticecenter/csgjcr")

# Load csgjcr package
library(csgjcr)

# Save SharePoint path
sp_data_path <- csgjcr::csg_sp_path(file.path("50 State Revocations Project",
                                              "50 State Survey (2022)"))

###################    Attention    #####################

#####
# Load remaining packages
#####
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
  no_data_values <- c("n/a", "na", "nodata", "no data", "notavailable",
                      "not available", "nr", "notready", "not ready", "[none]", "none")

  df1 <- janitor::clean_names(df) %>%
    select(year_2019 = x6, year_2020 = x7, year_2021 = x8) %>%
    slice(46) %>%
    mutate(
      across(everything(), ~str_to_lower(as.character(.))),
      across(everything(), ~replace(., . %in% no_data_values, "No Data")),
      state = state_name
    )
  return(df1)
}

# Extract admissions and population data
fnc_extract_data <- function(df, state_name) {
  no_data_values <- c("n/a", "na", "nodata", "no data", "notavailable",
                      "not available", "nr", "notready", "not ready", "[none]", "none")

  df1 <- janitor::clean_names(df) %>%
    mutate(metric = coalesce(!!!select(., 1:3))) %>%
    select(
      metric,
      year_2018 = x5,
      year_2019 = x6,
      year_2020 = x7,
      year_2021 = x8
    ) %>%
    slice(c(22:31, 34:43)) %>%
    mutate(
      across(everything(), ~str_to_lower(as.character(.))),
      across(year_2018:year_2021, ~replace(., . %in% no_data_values, "No Data")),
      across(year_2018:year_2020, ~ifelse(is.na(.) | . == "null", "No Data", .)),
      year_2021 = ifelse(is.na(year_2021) | year_2021 == "null", "Left Blank", year_2021),
      state = state_name
    )
  return(df1)
}

# Filter and select data
fnc_filter_and_select <- function(data, year) {
  data %>% filter(year == !!year)
}
