############################################
# Project:  MCLC Survey (2022)
# File: format_data.R
# Last updated: April 18, 2023 (MAR)
# Author: Mari Roberts

# Format MCLC survey data
############################################

################################################################################

# Get costs

################################################################################

# Create empty list
df_final <- list()

costs <- map(.x = states,  .f = function(x) {
  df_state <- state_dfs[[x]]
  df_final[x] <- fnc_extract_costs(df_state, x)
})

# Change list into a data frame
costs <- bind_rows(costs)

# Replace strings concerning no data to NA
costs[costs == "No Data" ]    <- NA
costs[costs == "null" ]       <- NA
costs[costs == "Left Blank" ] <- NA

################################################################################

# Get admissions and population data

################################################################################

# Create empty list
df_final <- list()

# Run custom function to extract admissions and population data
# Ignore warnings - can't make NA's numeric so there is a warning
state_data_all <- map(.x = states,  .f = function(x) {
  df_state <- state_dfs[[x]]
  df_final[x] <- fnc_extract_data(df_state, x)
})

# change list into a data frame and arrange columns
state_data_all <- bind_rows(state_data_all)

# remove commas and make data numeric
state_data_all <- state_data_all %>%
  mutate(year_2018 = as.numeric(gsub(",","",year_2018)),
         year_2019 = as.numeric(gsub(",","",year_2019)),
         year_2020 = as.numeric(gsub(",","",year_2020)),
         year_2021 = as.numeric(gsub(",","",year_2021)))

################################################################################

# Format data so each year and type (adm vs pop) is an excel sheet. E.g. Admissions 2021

################################################################################

# Reshape data so variables are columns and states are rows
state_data <- gather(state_data_all, year, total, year_2018:year_2021, factor_key=TRUE)
state_data <- spread(state_data, metric, total)

# Order variables
state_data <- state_data  %>% clean_names() %>%
  mutate(year = str_remove_all(year, "year_")) %>%
  select(state,
         year,

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
         new_offense_parole_violation_population)

# Create "sheet" for each adm data year
# Same format as last year
adm_2018 <- state_data %>% filter(year == 2018) %>%
  select(state, year, total_prison_admissions:new_offense_parole_violation_admissions)
adm_2019 <- state_data %>% filter(year == 2019) %>%
  select(state, year, total_prison_admissions:new_offense_parole_violation_admissions)
adm_2020 <- state_data %>% filter(year == 2020) %>%
  select(state, year, total_prison_admissions:new_offense_parole_violation_admissions)
adm_2021 <- state_data %>% filter(year == 2021) %>%
  select(state, year, total_prison_admissions:new_offense_parole_violation_admissions)

pop_2018 <- state_data %>% filter(year == 2018) %>%
  select(state, year, total_prison_population:new_offense_parole_violation_population)
pop_2019 <- state_data %>% filter(year == 2019) %>%
  select(state, year, total_prison_population:new_offense_parole_violation_population)
pop_2020 <- state_data %>% filter(year == 2020) %>%
  select(state, year, total_prison_population:new_offense_parole_violation_population)
pop_2021 <- state_data %>% filter(year == 2021) %>%
  select(state, year, total_prison_population:new_offense_parole_violation_population)

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

# Save data to sharepoint (last version: mclc_data_2022_04_13_2023.xlsx)
# # write.xlsx(mclc_data_2022, file = paste0(sp_data_path, "/50 State Survey (2022)/Data/mclc_data_2022_04_18_2023.xlsx"))
