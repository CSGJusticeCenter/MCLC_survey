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

# determine whether someone left data blank in the admissions section
state_adm <- adm_table_checklist %>% filter(state == "Iowa")
left_blank_adm <- any(state_adm=="Left Blank")

# determine whether someone left data blank in the population section
state_pop <- pop_table_checklist %>% filter(state == "Iowa")
left_blank_pop <- any(state_pop=="Left Blank")

# determine whether someone left data blank in the costs section
state_costs <- costs_table_checklist %>% filter(state == "Iowa")
left_blank_costs <- any(state_costs=="Left Blank")

sentence <- case_when(
  left_blank_adm == TRUE & left_blank_pop == TRUE  & left_blank_costs == TRUE   ~ "We noticed that you left some fields blank in the Admisisons, Population, and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form.",
  left_blank_adm == TRUE & left_blank_pop == FALSE & left_blank_costs == TRUE   ~ "We noticed that you left some fields blank in the Admisisons and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form.",
  left_blank_adm == TRUE & left_blank_pop == FALSE & left_blank_costs == FALSE  ~ "We noticed that you left some fields blank in the Admisisons section. If you do not have data for these fields, please input NA in the fields in your data collection form.",

  left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == FALSE ~ "",
  left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == TRUE  ~ "We noticed that you left some fields blank in the Population and Costs Previously Submitted sections. If you do not have data for these fields, please input NA in the fields in your data collection form.",
  left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == TRUE  ~ "We noticed that you left some fields blank in Costs Previously Submitted section. If you do not have data for these fields, please input NA in the fields in your data collection form."
)
