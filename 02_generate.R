############################################
# Project:  MCLC Survey (2022)
# File: generate.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Save state contact info
# Generate state checklists
# Checklist for data, definitions, costs, notes
############################################

state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final <- fnc_create_state_data_checklist(df_state, x)
})

# load package
library(purrr)

# create list of state forms
dfs <- list(Alabama, Idaho, Iowa, Missouri, Pennsylvania)
dfs <- setNames(dfs,c("Alabama", "Idaho", "Iowa", "Missouri", "Pennsylvania"))
states <- c("Alabama", "Idaho", "Iowa", "Missouri", "Pennsylvania")

# # create empty list
# state_data_checklist <- list()
#
# # generate state data checklists for each state and store as a list
# for(i in 1:length(dfs)){
#   df_state <- dfs[[i]]
#   count <- nrow(df_state)
#   state_data_checklist[i] <- count
# }

state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_create_state_data_checklist(df_state, x)
})

# change list into a data frame
state_data_checklist <- bind_rows(state_data_checklist)

