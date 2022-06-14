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
# each sheet is a state
alabama <- read_sheet('https://docs.google.com/spreadsheets/d/1SXBqghzBE5wgOzt7zs52yhK1oDw0R85gysyiKH0RRw0/edit?usp=sharing')