#######################################
# MCLC Survey
# Imports/cleans MCLC Survey for Automated Reports
# by MR/JSM
# 10/18/2022
#######################################

# load necessary packages
library(dplyr)
library(readr)
library(reshape)
library(readxl)
library(tidyverse)
library(data.table)
library(formattable)
library(scales)
library(mice)
library(VIM)
library(finalfit)
library(janitor)
library(Hmisc)
library(eeptools)

dataname <- "C:/Users/jmallett/The Council of State Governments/JC Research - Documents/50 State Revocations Project/50 State Survey (2022)/Data/mclc_data_2022_v2.xlsx"

################################
# COSTS: read cost data for 2019-2021
################################
costs        <- read_xlsx(dataname, sheet = "Costs",           .name_repair = "universal") %>%
  mutate_at(vars(-c("state")), as.numeric) %>%
  dplyr::rename(Cost.in.2019 = year_2019,
                Cost.in.2020 = year_2020,
                Cost.in.2021 = year_2021)

# read excel population/admissions data for 2018-2021
population18 <- read_xlsx(dataname, sheet = "Population 2018", .name_repair = "universal")
population19 <- read_xlsx(dataname, sheet = "Population 2019", .name_repair = "universal")
population20 <- read_xlsx(dataname, sheet = "Population 2020", .name_repair = "universal")
population21 <- read_xlsx(dataname, sheet = "Population 2021", .name_repair = "universal")

admissions18 <- read_xlsx(dataname, sheet = "Admissions 2018", .name_repair = "universal")
admissions19 <- read_xlsx(dataname, sheet = "Admissions 2019", .name_repair = "universal")
admissions20 <- read_xlsx(dataname, sheet = "Admissions 2020", .name_repair = "universal")
admissions21 <- read_xlsx(dataname, sheet = "Admissions 2021", .name_repair = "universal")

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
  select(-c(Total.New.Offense.Population,Total.Technical.Violation.Population)) %>%
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

# add labels
var.labels = c(states                                     = "State name", 
               year                                       = "Year",
               Total.Prison.Population                    = "Total population",
               Total.Supervision.Violation.Population     = "Total probation and parole violation population",
               Probation.Violation.Population             = "Total probation violation population (new offense + technical)",
               New.Offense.Probation.Violation.Population = "New offense probation violation population",
               Technical.Probation.Violation.Population   = "Technical probation violation population",
               Parole.Violation.Population                = "Total parole violation population (new offense + technical)",
               New.Offense.Parole.Violation.Population    = "New offense parole violation population",
               Technical.Parole.Violation.Population      = "Technical parole violation population")

label(population) = as.list(var.labels[match(names(population), names(var.labels))])

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
  select(-c(Total.New.Offense.Admissions,Total.Technical.Violation.Admissions)) %>% 
  mutate_at(vars(-c("state", "year")), decomma) %>%
  mutate_at(vars(year),list(factor)) %>%
  dplyr::rename(states = state)

# add labels
var.labels = c(states                                     = "State name", 
               year                                       = "Year",
               Total.Prison.Admissions                    = "Total admissions",
               Total.Supervision.Violation.Admissions     = "Total probation and parole violation admissions",
               Probation.Violation.Admissions             = "Total probation violation admissions (new offense + technical)",
               New.Offense.Probation.Violation.Admissions = "New offense probation violation admissions",
               Technical.Probation.Violation.Admissions   = "Technical probation violation admissions",
               Parole.Violation.Admissions                = "Total parole violation admissions (new offense + technical)",
               New.Offense.Parole.Violation.Admissions    = "New offense parole violation admissions",
               Technical.Parole.Violation.Admissions      = "Technical parole violation admissions")

label(admissions) = as.list(var.labels[match(names(admissions), names(var.labels))])

# merge together, set up tables for change
adm_pop_analysis <- merge(admissions, population, by = c("states","year")) %>% 
  select(states, year, everything()) %>% 
  arrange(desc(states))
