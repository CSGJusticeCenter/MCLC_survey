############################################
# Project:  MCLC Survey (2022)
# File: import.R
# Last updated: June 9, 2022
# Author: Mari Roberts

# Load data directly from Google Sheets
############################################

# load packages
library(googlesheets4)

# Use thi code to connect to gdrive, fetch new token, and authorize tidyverse API
# googlesheets4::gs4_deauth()
# googlesheets4::gs4_auth()

# read google sheets data into R
x <- read_sheet('https://docs.google.com/spreadsheets/d/1xP5TUaEJ5My4djXqytVw5RKWHMsG1Vz9ejQ3oU4Mros/edit?usp=sharing')
