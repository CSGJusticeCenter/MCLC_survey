#######################################
# MCLC Survey
# Imports/cleans MCLC Survey for Automated Reports
# by MR/JSM
# 10/18/2022
#######################################

readin <- paste0(sp_data_path,dataname)

################################
# COSTS: read cost data for 2019-2021
################################
costs        <- read_xlsx(readin, sheet = "Costs",           .name_repair = "universal") %>%
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
  select(-c(total_new_offense_population,total_technical_violation_population)) %>%
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

# add labels
var.labels = c(states                                     = "State name",
               year                                       = "Year",
               total_prison_population                    = "Total population",
               total_supervision_violation_population     = "Total probation and parole violation population",
               probation_violation_population             = "Total probation violation population (new offense + technical)",
               new_offense_probation_violation_population = "New offense probation violation population",
               technical_probation_violation_population   = "Technical probation violation population",
               parole_violation_population                = "Total parole violation population (new offense + technical)",
               new_offense_parole_violation_population    = "New offense parole violation population",
               technical_parole_violation_population      = "Technical parole violation population")

population = upData(population, labels = var.labels)

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
  select(-c(total_new_offense_admissions,total_technical_violation_admissions)) %>%
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

# add labels
var.labels = c(states                                     = "State name",
               year                                       = "Year",
               total_prison_admissions                    = "Total admissions",
               total_supervision_violation_admissions     = "Total probation and parole violation admissions",
               probation_violation_admissions             = "Total probation violation admissions (new offense + technical)",
               new_offense_probation_violation_admissions = "New offense probation violation admissions",
               technical_probation_violation_admissions   = "Technical probation violation admissions",
               parole_violation_admissions                = "Total parole violation admissions (new offense + technical)",
               new_offense_parole_violation_admissions    = "New offense parole violation admissions",
               technical_parole_violation_admissions      = "Technical parole violation admissions")

admissions = upData(admissions, labels = var.labels)

# merge together, set up tables for change
adm_pop_analysis <- merge(admissions, population, by = c("states","year")) %>%
  select(states, year, everything()) %>%
  arrange(desc(states))
