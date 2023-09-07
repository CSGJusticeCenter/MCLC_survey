---
title:  "MCLC National Estimates: 2018-2021"
author: "Research Division"
date:   "10/20/2022"
output:
  html_document
---

  # {.tabset .tabset-fade .tabset-pills}

#######################################
# PROJECT: More Community, Less Confinement (2022)
# PURPOSE: Impute values, national estimates, state estimates and costs
# AUTHOR: JSM
#######################################

# Get data, clean, and reformat
source("code/survey/00_library_functions.R")
source("code/survey/01_import.R")
source("code/survey/02_format_data.R")
source("code/survey/03_clean.R")

##SETUP
#years of data
year     <- c('2018','2019','2020','2021')
yearnum  <- c(1,     2,     3,     4)
refyear  <- c('2019','2020','2021')
refnum   <- 1:3

#number of survey variables
numvar   <- 1:8

#labels
var.labels = c(year                                = "Year",
               overall_admissions                  = "Overall admissions",
               admissions_for_violations           = "Admissions for violations",
               admissions_for_technical_violations = "Admissions for technical violations",
               admissions_for_new_crime_violations = "Admissions for new crime violations",
               overall_population                  = "Overall population",
               violator_population                 = "Violator population",
               technical_violator_population       = "Technical violator population",
               new_crime_violator_population       = "New crime violator population"
)

################################################################################
# IMPUTATION
# MICE
# National Estimates w/95% CI's
################################################################################

#calculate imputed values (multiple imputation)
source("code/survey/MImp1.R")

######
#produce confidence intervals
#post multiple imputation t-test
#grab response columns for creating estimated counts/CIs
######
#get response columns from survey
#there are 8 response columns
tablevals<- colnames(mice_imputed_data1[numvar[3]:(length(numvar)+2)])

source("code/survey/MImp2.R")

##############################################################
##################CREATE A TABLE HERE!!!!!!!!!!!!#############
#combine & print
cbind(nat.2018all,natCI.2018all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2019all,natCI.2019all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2020all,natCI.2020all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2021all,natCI.2021all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
##############################################################
##############################################################


#############################################################################
###############CALCULATING STATE ESTIMATES
#############################################################################

source("code/survey/MImp3.R")

# save data to sharepoint
write.csv(national.est,  file=paste0(sp_data_path,"/mclc_data_2022_with_imputatation.csv"))

#############################
###COSTS#####################

source("code/survey/Costs.R")
#averted cost total
sum(cost.final$averted_costs19_20, na.rm = TRUE) #2019 - 2020
sum(cost.final$averted_costs19_21, na.rm = TRUE) #2019 - 2021
sum(cost.final$averted_costs20_21, na.rm = TRUE) #2020 - 2021
