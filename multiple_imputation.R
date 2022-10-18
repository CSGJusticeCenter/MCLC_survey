#######################################
# PROJECT: More Community, Less Confinement (2022)
# PURPOSE: Impute values, national estimates, state estimates and costs
# AUTHOR: JSM
#######################################

# read automated_clean to get data
source("clean.R")

#rename columns
names(admissions)[names(admissions) == "States"] <- "states"
names(population)[names(population) == "States"] <- "states"

# merge together, set up tables for change
adm_pop_analysis <- merge(admissions, population, by = c("states","year")) %>% 
  select(states, year, everything()) %>% 
  arrange(desc(states))

##SETUP
#years of data
year     <- c('2018','2019','2020')
yearnum  <- c(1,     2,     3)
refyear  <- c('2019','2020')
refnum   <- 1:2

#number of survey variables
numvar  <- 1:8

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
source("MImp1.R")

######
#produce confidence intervals
#post multiple imputation t-test
#grab response columns for creating estimated counts/CIs
######

#get response columns from survey
#there are 8 response columns
tablevals<- colnames(mice_imputed_data1[numvar[3]:(length(numvar)+2)])

source("MImp2.R")

##############################################################
##################CREATE A TABLE HERE!!!!!!!!!!!!#############
#combine & print
cbind(nat.2018all,natCI.2018all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2019all,natCI.2019all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
cbind(nat.2020all,natCI.2020all) %>%
  mutate(across(where(is.numeric),formattable::comma,1))
##############################################################
##############################################################


#############################################################################
###############CALCULATING STATE ESTIMATES
#
#
#############################################################################

source("MImp3.R")

#############################
###COSTS#####################

source("Costs.R")
#averted cost total
sum(cost.final$averted_costs)
