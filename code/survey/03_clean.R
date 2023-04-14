#######################################
# MCLC Survey
# Imports/cleans MCLC Survey for Automated Reports
# by MR/JSM
# Last Updated: April 13, 2023 (MAR)

# Version History:
# Version 3: Data from Google sheets
# Version 4: Manual changes by making Maine's new offense admissions NA in Excel
# Version 5: Replace version 4 total admissions and population with BJS numbers - not using anymore bc we're only removing MCLC data for some states not all of them
# Version 6: Replace version 4 total admissions and population with BJS numbers for specific states flagged in Comparison_MCLC_and_BJS_data_v1.xlsx
# Version 7: Replace version 4 total admissions and population with BJS numbers for just New Mexico, Nebraska, and Alaska (population only)
# Version 8: Re-read data from Google Sheets because of Nebraska's new submission. Replace Maine's new offense admissions with NA.
#            Replace total admissions and population with BJS numbers for just New Mexico and Alaska (2021 population only).

# Input: mclc_data_2022_04_13_2023.xlsx - re-pulled data from Google Sheets because of Nebraska's submission on 4/13/23
# Final: version 8 of data (4/13/2023)
#######################################

# Will create latest version at the end of this file
readin <- "C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Data/mclc_data_2022_04_13_2023.xlsx"
#readin <- "C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Data/mclc_data_2022_04_13_2023.xlsx"

# Get info on whether to use BJS or MCLC data by state and admissions vs population
comparison_bjs_mclc_adm.xlsx <- read_excel("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Comparison_MCLC_and_BJS_data_v1.xlsx", sheet = "Admissions", skip = 2, col_names = TRUE)
comparison_bjs_mclc_pop.xlsx <- read_excel("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Comparison_MCLC_and_BJS_data_v1.xlsx", sheet = "Populations", skip = 1, col_names = TRUE)
#comparison_bjs_mclc_adm.xlsx <- read_excel("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Comparison_MCLC_and_BJS_data_v1.xlsx", sheet = "Admissions", skip = 2, col_names = TRUE)
#comparison_bjs_mclc_pop.xlsx <- read_excel("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Comparison_MCLC_and_BJS_data_v1.xlsx", sheet = "Populations", skip = 1, col_names = TRUE)

# Import BJS total admissions and population since these numbers are more reliable
bjs_pop.xlsx <- read_excel("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Data/BJS - Prison Year-End Populations - 1978 to current.xlsx")
bjs_adm.xlsx <- read_excel("C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Data/BJS - Prison Admissions & Releases - 1978 to current.xlsx")
#bjs_pop.xlsx <- read_excel("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Data/BJS - Prison Year-End Populations - 1978 to current.xlsx")
#bjs_adm.xlsx <- read_excel("C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Data/BJS - Prison Admissions & Releases - 1978 to current.xlsx")



################################
# COSTS: read cost data for 2019-2021
################################

costs        <- read_xlsx(readin, sheet = "Costs", .name_repair = "universal") %>%
  mutate_at(vars(-c("state")), as.numeric) %>%
  dplyr::rename(Cost.in.2019 = year_2019,
                Cost.in.2020 = year_2020,
                Cost.in.2021 = year_2021)

# read excel population/admissions data for 2018-2021
population18 <- read_xlsx(readin, sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx(readin, sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx(readin, sheet = "Population 2020", .name_repair = "universal")
population21 <- read_xlsx(readin, sheet = "Population 2021", .name_repair = "universal")

admissions18 <- read_xlsx(readin, sheet = "Admissions 2018", .name_repair = "universal")
admissions19 <- read_xlsx(readin, sheet = "Admissions 2019", .name_repair = "universal")
admissions20 <- read_xlsx(readin, sheet = "Admissions 2020", .name_repair = "universal")
admissions21 <- read_xlsx(readin, sheet = "Admissions 2021", .name_repair = "universal")

##############
# Population
##############

# add year variable
population18$year <- "2018"
population19$year <- "2019"
population20$year <- "2020"
population21$year <- "2021"

# combine pop data
population <- rbind(population18, population19, population20, population21) %>%
  mutate(total_new_offense_violation_population = total_new_offense_population) %>%
  select(-c(total_new_offense_population)) %>%
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

##############
# Admissions
##############

# add year variable
admissions18$year <- "2018"
admissions19$year <- "2019"
admissions20$year <- "2020"
admissions21$year <- "2021"

# combine pop data
admissions <- rbind(admissions18, admissions19, admissions20, admissions21) %>%
  mutate(total_new_offense_violation_admissions = total_new_offense_admissions) %>%
  select(-c(total_new_offense_admissions)) %>%
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

##############
# Admissions and Population
##############

# merge together, set up tables for change
adm_pop_analysis <- merge(admissions, population, by = c("states","year")) %>%
  select(states, year, everything()) %>%
  arrange(desc(states)) %>%
  mutate(year = as.character(year))

################################
# BJS vs MCLC data: will use total admissions and population for certain states
################################

# BJS admissions
# Rename variables
comparison_bjs_mclc_adm <- comparison_bjs_mclc_adm.xlsx %>%
  clean_names() %>%
  select(states = x1,
         bjs_adm_18 = total_adms_2,
         bjs_adm_19 = total_adms_5,
         bjs_adm_20 = total_adms_8,
         bjs_adm_21 = total_adms_11) %>%
  distinct()

# If MCLC numbers should be used, assign MCLC numbers as final numbers, otherwise use BJS numbers
comparison_bjs_mclc_adm <- comparison_bjs_mclc_adm %>%
  select(states, bjs_adm_18:bjs_adm_21) %>%
  pivot_longer(c("bjs_adm_18", "bjs_adm_19", "bjs_adm_20", "bjs_adm_21")) %>%
  mutate(year = case_when(
    name == "bjs_adm_18" ~ 2018,
    name == "bjs_adm_19" ~ 2019,
    name == "bjs_adm_20" ~ 2020,
    name == "bjs_adm_21" ~ 2021
  )) %>%
  mutate(adm_or_pop = "Admissions") %>%
  select(states,
         year,
         total_prison_admissions_bjs = value) %>%
  mutate(year = as.character(year))

# BJS Population
# Rename variables
comparison_bjs_mclc_pop <- comparison_bjs_mclc_pop.xlsx %>%
  clean_names() %>%
  select(states = x1,
         bjs_pop_18 = x2018_2,
         bjs_pop_19 = x2019_3,
         bjs_pop_20 = x2020_4,
         bjs_pop_21 = x2021_5) %>%
  distinct()

# If MCLC numbers should be used, assign MCLC numbers as final numbers, otherwise use BJS numbers
comparison_bjs_mclc_pop <- comparison_bjs_mclc_pop %>%
  select(states, bjs_pop_18:bjs_pop_21) %>%
  pivot_longer(c("bjs_pop_18", "bjs_pop_19", "bjs_pop_20", "bjs_pop_21")) %>%
  mutate(year = case_when(
    name == "bjs_pop_18" ~ 2018,
    name == "bjs_pop_19" ~ 2019,
    name == "bjs_pop_20" ~ 2020,
    name == "bjs_pop_21" ~ 2021
  )) %>%
  mutate(adm_or_pop = "Population") %>%
  select(states,
         year,
         total_prison_population_bjs = value) %>%
  mutate(year = as.character(year))

##############
# Replace total admissions and total population with BJS numbers for specific states (use_mclc_vs_bjs), which are more reliable
##############

# Replace total admissions and total population with BJS numbers for specific states
adm_pop_analysis_with_bjs <- adm_pop_analysis %>%
  left_join(comparison_bjs_mclc_adm, by = c("states", "year")) %>%
  left_join(comparison_bjs_mclc_pop, by = c("states", "year")) %>%
  mutate(
    total_prison_population = case_when(states == "Alaska" &
                                          year == 2021         ~ total_prison_population_bjs,
                                        states == "New Mexico" ~ total_prison_population_bjs,
                                        TRUE                   ~ total_prison_population),
    total_prison_admissions = case_when(states == "New Mexico" ~ total_prison_admissions_bjs,
                                        TRUE                   ~ total_prison_admissions)
  ) %>%
  select(-c(total_prison_population_bjs,total_prison_admissions_bjs))

# Make Maine's new offense violations NA
adm_pop_analysis_with_bjs <- adm_pop_analysis_with_bjs %>%
  mutate(total_new_offense_violation_admissions = ifelse(states == "Maine", NA, total_new_offense_violation_admissions))

# add labels
var.labels = c(states                                     = "State name",
               year                                       = "Year",
               total_prison_admissions                    = "Total admissions", overall_admissions = "Overall admissions",
               total_supervision_violation_admissions     = "Total probation and parole violation admissions",
               probation_violation_admissions             = "Total probation violation admissions",
               parole_violation_admissions                = "Total parole violation admissions",
               total_technical_violation_admissions       = "Total technical violation admissions",
               technical_probation_violation_admissions   = "Admissions for technical violations, probation", 
               technical_parole_violation_admissions      = "Admissions for technical violations, parole",
               total_new_offense_violation_admissions     = "Total new offense violation admissions",
               new_offense_probation_violation_admissions = "Admissions for new crime violations, probation",
               new_offense_parole_violation_admissions    = "Admissions for new crime violations, parole",
               
               total_prison_population                    = "Total population", overall_population = "Overall population",
               total_supervision_violation_population     = "Total probation and parole violation population",
               probation_violation_population             = "Total probation violation population",
               parole_violation_population                = "Total parole violation population",
               total_technical_violation_population       = "Total technical violation population",
               technical_probation_violation_population   = "Technical violator population, probation",
               technical_parole_violation_population      = "Technical violator population, parole",
               total_new_offense_violation_population     = "Total new offense violation population",
               new_offense_probation_violation_population = "New crime violator population, probation",
               new_offense_parole_violation_population    = "New crime violator population, parole"
               )

adm_pop_analysis_with_bjs = upData(adm_pop_analysis_with_bjs, labels = var.labels)

# Save data to sharepoint (most recent version: version 8 on 4/13/2023)
# write.xlsx(adm_pop_analysis_with_bjs, file = "C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Data/mclc_data_2022_TEST.xlsx")
# write.xlsx(adm_pop_analysis_with_bjs, file = "C:/Users/mroberts/The Council of State Governments/JC Research - 50 State Revocations Project/50 State Survey (2022)/Data/mclc_data_2022_v8.xlsx")
