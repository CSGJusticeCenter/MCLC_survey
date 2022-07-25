############################################
# Project:  MCLC Survey (2022)
# File: generate.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Save state contact info
# Generate state checklists
# Checklist for data, definitions, costs, notes
############################################

library(purrr)

# create list of state forms
dfs <- list(alabama.xlsx, alaska.xlsx)
dfs <- setNames(dfs,c("alabama.xlsx","alaska.xlsx"))
dfs_names <- c("alabama.xlsx", "alaska.xlsx")

# generate state data checklist for each state
state_data_checklist <- map(.x = dfs_names,  .f = function(x) {

  df_state <- dfs[[x]]
  df_final <- fnc_create_state_data_checklist(df_state, x)

})

# change list into a data frame
state_data_checklist <- bind_rows(state_data_checklist)
