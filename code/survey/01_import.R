############################################
# Project:  MCLC Survey (2022)
# File: import.R
# Last updated: September 7, 2023

# Load data directly from Google sheets
# Each state has it's own Google sheet
# Previous survey file: mclc_data_2021_v13.xlsx
# BJS data: BJS - Prison Year-End Populations - 1978 to current.xlsx
#           BJS - Prison Admissions & Releases - 1978 to current.xlsx
############################################

#####
# 2021 survey
#####

# Import survey data submitted in 2021 to compare with new submissions in 2022.
# This way we will know who changed their data for 2018, 2019, and 2020.
# Read in each admissions and population sheet from the xlsx.
adm18 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Admissions 2018")
adm19 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Admissions 2019")
adm20 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Admissions 2020")
pop18 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Population 2018")
pop19 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Population 2019")
pop20 <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Population 2020")

# Import costs.
costs <- read_excel(paste0(sp_data_path, "/Data/mclc_data_2021_v13.xlsx"), sheet = "Costs")

#####
# 2022 survey
#####

# Google form links
form_links <- read_excel(paste0(sp_data_path, "/MCLC Overview.xlsx", sep = ""))
form_links <- form_links %>% clean_names() %>%
  select(state, form_link = google_sheet_link) %>%
  mutate(state = str_replace_all(state, " \\(Excel\\)", ""))

#####
# BJS data - use total admissions and population instead
#####

# Import BJS total admissions and population since these numbers are more reliable
bjs_pop.xlsx <- read_excel(paste0(sp_data_path, "/Data/BJS - Prison Year-End Populations - 1978 to current.xlsx"))
bjs_adm.xlsx <- read_excel(paste0(sp_data_path, "/Data/BJS - Prison Admissions & Releases - 1978 to current.xlsx"))

# Get info on whether to use BJS or MCLC data by state and admissions vs population
comparison_bjs_mclc_adm.xlsx <- read_excel(paste0(sp_data_path, "/Comparison_MCLC_and_BJS_data_v1.xlsx"), sheet = "Admissions", skip = 2, col_names = TRUE)
comparison_bjs_mclc_pop.xlsx <- read_excel(paste0(sp_data_path, "/Comparison_MCLC_and_BJS_data_v1.xlsx"), sheet = "Populations", skip = 1, col_names = TRUE)

###################    Attention    #####################

# # Use this code to connect to Google Drive, fetch new token, and authorize tidyverse API
# # Make sure the sheets have been officially shared with you and you have editing access
# googlesheets4::gs4_deauth()
# googlesheets4::gs4_auth()

###################    Attention    #####################

# Import Google Sheets data
# Initialize an empty list to store data frames
state_dfs <- list()

# Loop through each row of form_links to read Google Sheets
for(i in 1:nrow(form_links)) {
  state <- form_links$state[i]
  form_link <- form_links$form_link[i]

  # Convert the state name to a valid variable name (e.g., "New York" to "New_York")
  state_var_name <- make.names(state)

  # Read Google Sheet
  state_data <- read_sheet(form_link)

  # Add the data frame to the list, named by the state
  state_dfs[[state_var_name]] <- state_data
}

