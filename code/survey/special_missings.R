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
    #MAINE (ADMISSIONS ONLY)- all parole variables get 0
    #                         set total new offenses to missing (already conducted in clean_03.R program)
    #                         back calculation new offense probation violations from probation violation total and technical probation violations
    parole_violation_admissions                = case_when(
      states == 'Maine' ~ 0,
      TRUE ~ as.numeric(parole_violation_admissions)),
    technical_parole_violation_admissions      = case_when(
      states == 'Maine' ~ 0,
      TRUE ~ as.numeric(technical_parole_violation_admissions)),
    new_offense_parole_violation_admissions    = case_when(
      states == 'Maine' ~ 0,
      TRUE ~ as.numeric(new_offense_parole_violation_admissions)),
    new_offense_probation_violation_admissions = case_when(
      states == 'Maine' ~ as.numeric(probation_violation_admissions - technical_probation_violation_admissions),
      TRUE ~ as.numeric(new_offense_probation_violation_admissions)),
    
    #WASHINGTON- all lowest aggregations should be missing
    #            next level aggr. should be used instead of sum
    probation_violation_admissions             = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ probation_violation_admissions),
    parole_violation_admissions                = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ parole_violation_admissions),
    technical_parole_violation_admissions      = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ technical_parole_violation_admissions),
    technical_probation_violation_admissions   = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ technical_probation_violation_admissions),
    new_offense_parole_violation_admissions    = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ new_offense_parole_violation_admissions),
    new_offense_probation_violation_admissions = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ new_offense_probation_violation_admissions),
    
    probation_violation_population             = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ probation_violation_population),
    parole_violation_population                = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ parole_violation_population),
    technical_parole_violation_population      = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ technical_parole_violation_population),
    technical_probation_violation_population   = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ technical_probation_violation_population),
    new_offense_parole_violation_population    = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ new_offense_parole_violation_population),
    new_offense_probation_violation_population = case_when(
      states == 'Washington' ~ NA,
      TRUE ~ new_offense_probation_violation_population)
  )

#check calculations for all states (except Maine/Washington, which have special cases)
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
states77 <- c()

#for categorizing special missings
replaceNA <- function(x) {
  x = ifelse(is.na(x) & adm_pop_analysis_with_bjs1$states %in% states99, -99, 
             ifelse(is.na(x) & adm_pop_analysis_with_bjs1$states %in% states88, -88, 
                    ifelse(is.na(x) & adm_pop_analysis_with_bjs1$states %in% states77, -77, x
                    )
             )
  )
  return(x)
}

#REQUIRES UPDATING EACH YEAR
replaceNA.MAINE <- function(x) {
  x = ifelse(is.na(x) & adm_pop_analysis_with_bjs1$states == "Maine", -88, x)
}

#create special missings and store as dataframe
specialmissings <- adm_pop_analysis_with_bjs1 %>%
  mutate(
    #make NAs into special missings
    across(!states & !year, replaceNA),
    
    #REQUIRES UPDATING EACH YEAR
    #special cases for Maine
    parole_violation_admissions                = case_when(
      states == 'Maine' ~ -77,
      TRUE ~ as.numeric(parole_violation_admissions)),
    technical_parole_violation_admissions      = case_when(
      states == 'Maine' ~ -77,
      TRUE ~ as.numeric(technical_parole_violation_admissions)),
    new_offense_parole_violation_admissions    = case_when(
      states == 'Maine' ~ -77,
      TRUE ~ as.numeric(new_offense_parole_violation_admissions)),
    total_new_offense_violation_admissions    = case_when(
      states == 'Maine' ~ -88,
      TRUE ~ as.numeric(total_new_offense_violation_admissions)),
    across(c(total_supervision_violation_population,
             probation_violation_population,
             parole_violation_population,
             total_technical_violation_population,
             technical_probation_violation_population,
             technical_parole_violation_population,      
             new_offense_probation_violation_population,
             new_offense_parole_violation_population,
             total_new_offense_violation_population),
           replaceNA.MAINE
           )
  )