#REQUIRES UPDATING EACH YEAR
states99 <- c("Alaska", 
              "New Mexico")

#REQUIRES UPDATING EACH YEAR
states88 <- c("Alabama",
              "Connecticut",
              "Delaware",
              "Georgia",
              "Illinois",
              "Iowa",
              "Kentucky",
              "Maine",
              "Maryland",
              "Massachusetts",
              "Michigan",
              "Minnesota",
              "Nebraska", 
              "Nevada",
              "New Hampshire",
              "New Jersey",
              "New York",
              "North Dakota",
              "Ohio",
              "Oklahoma",
              "Pennsylvania",
              "South Carolina",
              "Texas",
              "Vermont",
              "Washington",
              "West Virginia")

#REQUIRES UPDATING EACH YEAR
states77 <- c('Delaware',
              'Maine')

#for categorizing special missings - ensure that 'whichstate' is mutually exclusive
fixreporting <- function(whichstate, missval, x) {
  x = ifelse(adm_pop_analysis_with_bjs1$states %in% whichstate, missval, x)
  return(x)
}







#CHECKS!!!###################################
for (i in 1:200) {
  CHECKSTATE <- ifelse(sum(grepl("TRUE",duplicated(t(adm_pop_analysis[i,])))) > 0, 1, 0)
  statename  <- adm_pop_analysis[i,]$states
  stateyear  <- adm_pop_analysis[i,]$year
  assign(paste0("valcheck",statename,stateyear),CHECKSTATE,envir = .GlobalEnv)
}

test<-mget(ls(pattern="valcheck"), .GlobalEnv)
test2<-data.frame(unlist(Filter(function(x) x == 1, test)))
test3<-add_rownames(test2,var="states") %>% select(states)
write.xlsx(test3, file = paste0(sp_data_path, "/50 State Survey (2022)/Data/checkstatesrepeatedvalues.xlsx"))
#############################################








#clean up missing data and incorrectly reported data
adm_pop_analysis_with_bjs1 <- adm_pop_analysis_with_bjs %>%
  mutate(
    #blank out 0s - there should never be a 0 value in the data (EXCEPT IF PAROLE HAS BEEN ABOLISHED - see below)
    across(!states & !year, ~replace(.,.== 0, NA)),
    
    #if the lowest level of aggregation is non-missing for both probation and parole (e.g., technical violation admissions probation/parole), then sum to create total (e.g., total technical violation admissions)
    #if the lowest level of aggregation is missing for either or both probation and parole (e.g., technical violation admissions probation/parole), then summed total is MISSING for total (e.g., total technical violation admissions)
    total_technical_violation_admissions   = case_when(
      states %nin% c('Maine','Washington') ~ as.numeric(technical_probation_violation_admissions   + technical_parole_violation_admissions),
      TRUE ~ as.numeric(total_technical_violation_admissions)),
    total_new_offense_violation_admissions = case_when(
      states %nin% c('Maine','Washington') ~ as.numeric(new_offense_probation_violation_admissions + new_offense_parole_violation_admissions),
      TRUE ~ as.numeric(total_new_offense_violation_admissions)),
    total_technical_violation_population   = case_when(
      states %nin% c('Maine','Washington') ~ as.numeric(technical_probation_violation_population   + technical_parole_violation_population),
      TRUE ~ as.numeric(total_technical_violation_population)),
    total_new_offense_violation_population = case_when(
      states %nin% c('Maine','Washington') ~ as.numeric(new_offense_probation_violation_population + new_offense_parole_violation_population),
      TRUE ~ as.numeric(total_new_offense_violation_population)),
    
    ##SPECIAL STATE CASES
    #MAINE/DELAWARE- all parole variables get 0
    #                set total new offenses to missing (already conducted in clean_03.R program)
    #                back calculation new offense probation violations from probation violation total and technical probation violations
    across(c(parole_violation_admissions,
             technical_parole_violation_admissions,
             new_offense_parole_violation_admissions,
             parole_violation_population,
             technical_parole_violation_population,
             new_offense_parole_violation_population
             ),
    fixreporting, whichstate = states77, missval = 0
    ),
    #back calculation of new offense probation violation admissions
    new_offense_probation_violation_admissions = case_when(
      states == 'Maine' ~ as.numeric(probation_violation_admissions - technical_probation_violation_admissions),
      TRUE ~ as.numeric(new_offense_probation_violation_admissions)),
    
    #WASHINGTON- all lowest aggregations should be missing
    #            next level aggr. should be used instead of sum
    across(c(probation_violation_admissions,
             parole_violation_admissions,
             technical_parole_violation_admissions,
             technical_probation_violation_admissions,
             new_offense_parole_violation_admissions,
             new_offense_probation_violation_admissions,
             
             probation_violation_population,
             parole_violation_population,
             technical_parole_violation_population,
             technical_probation_violation_population,
             new_offense_parole_violation_population,
             new_offense_probation_violation_population
             ),
           ~ifelse(adm_pop_analysis_with_bjs1$states == 'Washington', NA, .x)
           )
    )

#check summing calculations for all states (except Maine/Washington, which have special cases)
check.dat <- adm_pop_analysis_with_bjs1 %>%
  select(c("states","year",
           "total_technical_violation_admissions",  "technical_probation_violation_admissions",  "technical_parole_violation_admissions",
           "total_new_offense_violation_admissions","new_offense_probation_violation_admissions","new_offense_parole_violation_admissions",
           "total_technical_violation_population",  "technical_probation_violation_population",  "technical_parole_violation_population",
           "total_new_offense_violation_population","new_offense_probation_violation_population","new_offense_parole_violation_population"
            )
         ) %>%
  mutate(#main logic for states
         flag_error.ta = ifelse(states %nin% c("Washington", "Maine") & 
                                is.na(technical_probation_violation_admissions) & 
                                is.na(technical_parole_violation_admissions) &
                                !is.na(total_technical_violation_admissions), 1, 0),
         flag_error.na = ifelse(states %nin% c("Washington", "Maine") & 
                                  is.na(new_offense_probation_violation_admissions) & 
                                  is.na(new_offense_parole_violation_admissions) &
                                  !is.na(total_new_offense_violation_admissions), 1, 0),
         flag_error.tp = ifelse(states %nin% c("Washington", "Maine") & 
                                  is.na(technical_probation_violation_population) & 
                                  is.na(technical_parole_violation_population) &
                                  !is.na(total_technical_violation_population), 1, 0),
         flag_error.np = ifelse(states %nin% c("Washington", "Maine") & 
                                  is.na(new_offense_probation_violation_population) & 
                                  is.na(new_offense_parole_violation_population) &
                                  !is.na(total_new_offense_violation_population), 1, 0)
    
  )

#check for 0's in the data, which should exist only for Maine for Parole
check.zeroes <- which(adm_pop_analysis_with_bjs1 == 0, arr.ind=TRUE)
state.zeroes <- adm_pop_analysis_with_bjs1[check.zeroes[,1],1] #states with 0s
years.zeroes <- adm_pop_analysis_with_bjs1[check.zeroes[,1],2] #years with 0s
var.zeroes   <- names(adm_pop_analysis_with_bjs1[,check.zeroes[,2]]) #variables with 0s

#create dataframe with special missing values for analysis
#There are three types of special missings:
# -99 = REFUSED TO ANSWER SURVEY
##      A value of -99 is assigned for states that left data blank or submitted NAs (did not submit any data)
# -88 = MISSING DATA INTENTIONALLY
##      A value of -88 is assigned for states that submitted 0's, NA's or left data blank *intentionally*
# -77 = DATA DOESN'T EXIST
##      A value of -77 is assigned for states were parole was abolished

#for categorizing special missings - ensure that 'whichstate' is mutually exclusive
replaceNA <- function(whichstate, missval, x) {
  x = ifelse(is.na(x) & adm_pop_analysis_with_bjs1$states %in% whichstate, missval, x)
  return(x)
}

#for categorizing special missings for states without parole
replaceZERO <- function(whichstate, missval, x) {
  x = ifelse(x == 0 & adm_pop_analysis_with_bjs1$states %in% whichstate, missval, x)
  return(x)
}

#create special missings and store as dataframe
specialmissings <- adm_pop_analysis_with_bjs1 %>%
  mutate(
    #make NAs into special missings
    across(!states & !year, replaceNA, whichstate = states99, missval = -99),
    across(!states & !year, replaceNA, whichstate = states88, missval = -88),
    
    #REQUIRES UPDATING EACH YEAR
    #special cases for Maine and Delaware - change parole 0s to -77
    across(c(parole_violation_admissions,
             technical_parole_violation_admissions,
             new_offense_parole_violation_admissions,
             parole_violation_population,
             technical_parole_violation_population,
             new_offense_parole_violation_population
             ),
           replaceZERO, whichstate = states77, missval = -77
           )
    )






























