############################################
# Project:  MCLC Survey (2022)
# File: issues.R
# Last updated: August 18, 2022
# Author: Mari Roberts

# Check to see if data was left blank or
# if data doesn't add up correctly
# Determine wording depending on these findings
# This wording will be put in the email
############################################

##############
# Data quality checks - data was left blank
##############

# determine whether someone left data blank in the admissions section
state_adm <- adm_table_checklist %>% filter(state == "Pennsylvania")
left_blank_adm <- any(state_adm=="Left Blank")

# determine whether someone left data blank in the population section
state_pop <- pop_table_checklist %>% filter(state == "Pennsylvania")
left_blank_pop <- any(state_pop=="Left Blank")

# determine whether someone left data blank in the costs section
state_costs <- costs_table_checklist %>% filter(state == "Pennsylvania")
left_blank_costs <- any(state_costs=="Left Blank")

submission_quality_sentence <- case_when(
  left_blank_adm == TRUE & left_blank_pop == TRUE  & left_blank_costs == TRUE   ~ "We noticed that you left some fields blank in the Admisisons, Population, and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form. Please update your form accordingly.",
  left_blank_adm == TRUE & left_blank_pop == FALSE & left_blank_costs == TRUE   ~ "We noticed that you left some fields blank in the Admisisons and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form. Please update your form accordingly.",
  left_blank_adm == TRUE & left_blank_pop == FALSE & left_blank_costs == FALSE  ~ "We noticed that you left some fields blank in the Admisisons section. If you do not have data for these fields, please input NA in the fields in your data collection form. Please update your form accordingly.",

  left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == FALSE ~ "Thank you for submitting your data. Please review your data submission in the following tables. You may also update your submission using the button below.",
  left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == TRUE  ~ "We noticed that you left some fields blank in the Population and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form. Please update your form accordingly.",
  left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == TRUE  ~ "We noticed that you left some fields blank in the Costs Previously Submitted section. If you do not have data for these fields, please input NA in the fields in your data collection form. Please update your form accordingly."
)

##############
# Data quality checks - data doesn't add up correctly
##############

# filter to state
state_data_quality <- state_data_checklist %>% filter(state == "Pennsylvania")

# determine whether data doesn't add up correctly
check_supervision_violation_admissions_22     <- any(state_data_quality$check_supervision_violation_admissions_22     =="Doesn't Add Up")
check_probation_violation_admissions_22       <- any(state_data_quality$check_probation_violation_admissions_22       =="Doesn't Add Up")
check_parole_violation_admissions_22          <- any(state_data_quality$check_parole_violation_admissions_22          =="Doesn't Add Up")
check_new_offense_violation_admissions_22     <- any(state_data_quality$check_new_offense_violation_admissions_22     =="Doesn't Add Up")
check_total_technical_violation_admissions_22 <- any(state_data_quality$check_total_technical_violation_admissions_22 =="Doesn't Add Up")

check_supervision_violation_population_22     <- any(state_data_quality$check_supervision_violation_population_22     =="Doesn't Add Up")
check_probation_violation_population_22       <- any(state_data_quality$check_probation_violation_population_22       =="Doesn't Add Up")
check_parole_violation_population_22          <- any(state_data_quality$check_parole_violation_population_22          =="Doesn't Add Up")
check_new_offense_violation_population_22     <- any(state_data_quality$check_new_offense_violation_population_22     =="Doesn't Add Up")
check_total_technical_violation_population_22 <- any(state_data_quality$check_total_technical_violation_population_22 =="Doesn't Add Up")

# filter the data depending on data quality checks
# if supervision violation admissions != probation violation admissions + parole violation admissions, then select these variables to present in a table

####
# supervision violation admissions
####
{if(check_supervision_violation_admissions_22 == TRUE){
  df_supervision_violation_admissions <- state_data_quality %>%
    select(state, year, total_supervision_violation_admissions_22, probation_violation_admissions_22, parole_violation_admissions_22, check_supervision_violation_admissions_22)
}
else if(check_supervision_violation_admissions_22 == FALSE){
  df_supervision_violation_admissions <- ""
}}

####
# probation violation admissions
####
{if(check_probation_violation_admissions_22 == TRUE){
  df_probation_violation_admissions_22 <- state_data_quality %>%
    select(state, year, probation_violation_admissions_22, new_offense_probation_violation_admissions_22, technical_probation_violation_admissions_22, check_probation_violation_admissions_22)
}
else if(check_probation_violation_admissions_22 == FALSE){
  df_probation_violation_admissions_22 <- ""
}}

####
# parole violation admissions
####
{if(check_parole_violation_admissions_22 == TRUE){
  df_parole_violation_admissions_22 <- state_data_quality %>%
    select(state, year, parole_violation_admissions_22, new_offense_parole_violation_admissions_22, technical_parole_violation_admissions_22, check_parole_violation_admissions_22)
}
else if(check_parole_violation_admissions_22 == FALSE){
  df_parole_violation_admissions_22 <- ""
}}

####
# new offense violation admissions
####
{if(check_new_offense_violation_admissions_22 == TRUE){
  df_new_offense_violation_admissions_22 <- state_data_quality %>%
    select(state, year, total_new_offense_admissions_22, new_offense_probation_violation_admissions_22, technical_parole_violation_admissions_22, check_new_offense_violation_admissions_22)
}
else if(check_new_offense_violation_admissions_22 == FALSE){
  df_new_offense_violation_admissions_22 <- ""
}}

####
# technical violation admissions
####
{if(check_total_technical_violation_admissions_22 == TRUE){
  df_total_technical_violation_admissions_22 <- state_data_quality %>%
    select(state, year, total_technical_violation_admissions_22, technical_probation_violation_admissions_22, technical_parole_violation_admissions_22, check_total_technical_violation_admissions_22)
}
else if(check_total_technical_violation_admissions_22 == FALSE){
  df_total_technical_violation_admissions_22 <- ""
}}

####
# supervision violation population
####
{if(check_supervision_violation_population_22 == TRUE){
  df_supervision_violation_population <- state_data_quality %>%
    select(state, year, total_supervision_violation_population_22, probation_violation_population_22, parole_violation_population_22, check_supervision_violation_population_22)
}
  else if(check_supervision_violation_population_22 == FALSE){
    df_supervision_violation_population <- ""
  }}

####
# probation violation population
####
{if(check_probation_violation_population_22 == TRUE){
  df_probation_violation_population_22 <- state_data_quality %>%
    select(state, year, probation_violation_population_22, new_offense_probation_violation_population_22, technical_probation_violation_population_22, check_probation_violation_population_22)
}
  else if(check_probation_violation_population_22 == FALSE){
    df_probation_violation_population_22 <- ""
  }}

####
# parole violation population
####
{if(check_parole_violation_population_22 == TRUE){
  df_parole_violation_population_22 <- state_data_quality %>%
    select(state, year, parole_violation_population_22, new_offense_parole_violation_population_22, technical_parole_violation_population_22, check_parole_violation_population_22)
}
  else if(check_parole_violation_population_22 == FALSE){
    df_parole_violation_population_22 <- ""
  }}

####
# new offense violation population
####
{if(check_new_offense_violation_population_22 == TRUE){
  df_new_offense_violation_population_22 <- state_data_quality %>%
    select(state, year, total_new_offense_population_22, new_offense_probation_violation_population_22, technical_parole_violation_population_22, check_new_offense_violation_population_22)
}
  else if(check_new_offense_violation_population_22 == FALSE){
    df_new_offense_violation_population_22 <- ""
  }}

####
# technical violation population
####
{if(check_total_technical_violation_population_22 == TRUE){
  df_total_technical_violation_population_22 <- state_data_quality %>%
    select(state, year, total_technical_violation_population_22, technical_probation_violation_population_22, technical_parole_violation_population_22, check_total_technical_violation_population_22)
}
  else if(check_total_technical_violation_population_22 == FALSE){
    df_total_technical_violation_population_22 <- ""
  }}

