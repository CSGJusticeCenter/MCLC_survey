############################################
# Project:  MCLC Survey (2022)
# File: format_data.R
# Last updated: September 7, 2023
# Format MCLC survey data
############################################

################################################################################

# Get costs

################################################################################

# Get state names
states <- state.name

# Extract data and row-bind it into a data frame
costs <- purrr::map_dfr(states, function(x) {
  state_name <- make.names(x)
  df_state <- state_dfs[[state_name]]
  fnc_extract_costs(df_state, state_name)
})

# Replace specified strings with NA in one step
costs <- costs %>%
  mutate(across(everything(), ~na_if(., "No Data"))) %>%
  mutate(across(everything(), ~na_if(., "null"))) %>%
  mutate(across(everything(), ~na_if(., "Left Blank"))) %>%
  select(state, everything())

################################################################################

# Get admissions and population data

################################################################################

# Get state names
states <- state.name

# Extract data and row-bind it into a data frame
state_data_all <- purrr::map_dfr(states, function(x) {
  state_name <- make.names(x)
  df_state <- state_dfs[[state_name]]
  fnc_extract_data(df_state, state_name)
})

# Remove commas and make data numeric
# Generation of NA's expected (i.e., changes "Left Blank" to NA)
state_data_all <- state_data_all %>%
  mutate(across(starts_with("year_"), ~as.numeric(gsub(",", "", .))))


################################################################################

# Format data so each year and type (adm vs pop) is an excel sheet. E.g. Admissions 2021

################################################################################

state_data <- state_data_all %>%
  gather(key = "year", value = "total", year_2018:year_2021, factor_key = TRUE) %>%
  spread(key = "metric", value = "total") %>%
  clean_names() %>%
  mutate(year = as.numeric(str_remove_all(year, "year_"))) %>%
  select(
    state, year,
    # admissions
    total_prison_admissions,
    total_supervision_violation_admissions,
    probation_violation_admissions,
    parole_violation_admissions,
    total_technical_violation_admissions,
    technical_probation_violation_admissions,
    technical_parole_violation_admissions,
    total_new_offense_admissions,
    new_offense_probation_violation_admissions,
    new_offense_parole_violation_admissions,
    # population
    total_prison_population,
    total_supervision_violation_population,
    probation_violation_population,
    parole_violation_population,
    total_technical_violation_population,
    technical_probation_violation_population,
    technical_parole_violation_population,
    total_new_offense_population,
    new_offense_probation_violation_population,
    new_offense_parole_violation_population
  )

# Create "sheet" for each data year
# Admissions
adm_2018 <- state_data %>%
  filter(year == 2018) %>%
  select(state, year, ends_with("_admissions"))
adm_2019 <- state_data %>%
  filter(year == 2019) %>%
  select(state, year, ends_with("_admissions"))
adm_2020 <- state_data %>%
  filter(year == 2020) %>%
  select(state, year, ends_with("_admissions"))
adm_2021 <- state_data %>%
  filter(year == 2021) %>%
  select(state, year, ends_with("_admissions"))

# Create "sheet" for each population data year
# Population
pop_2018 <- state_data %>%
  filter(year == 2018) %>%
  select(state, year, ends_with("_population"))
pop_2019 <- state_data %>%
  filter(year == 2019) %>%
  select(state, year, ends_with("_population"))
pop_2020 <- state_data %>%
  filter(year == 2020) %>%
  select(state, year, ends_with("_population"))
pop_2021 <- state_data %>%
  filter(year == 2021) %>%
  select(state, year, ends_with("_population"))

# Write each data frame as a sheet into an excel workbook
mclc_data_2022 <- list('Admissions 2018' = adm_2018,
                       'Admissions 2019' = adm_2019,
                       'Admissions 2020' = adm_2020,
                       'Admissions 2021' = adm_2021,

                       'Population 2018' = pop_2018,
                       'Population 2019' = pop_2019,
                       'Population 2020' = pop_2020,
                       'Population 2021' = pop_2021,
                       'Costs'           = costs)

# Save data to sharepoint
# Define the folder and filename pattern to look for
folder_path <- paste0(sp_data_path, "/Data/")
# A pattern that specifically looks for filenames with timestamps
pattern <- "^mclc_pre_data_2022_\\d{8}_\\d{6}\\.xlsx$"

# Get a list of files that match the pattern
existing_files <- list.files(path = folder_path, pattern = pattern)

# Remove existing files with system time in their names
if (length(existing_files) > 0) {
  sapply(paste0(folder_path, existing_files), unlink)
}

# Get system time and format it
current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Generate filename with current time
file_name <- paste0(sp_data_path, "/Data/mclc_pre_data_2022_", current_time, ".xlsx")

# Write the Excel file
write.xlsx(mclc_data_2022, file = file_name)