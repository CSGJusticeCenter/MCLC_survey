############################################
# Project:  MCLC Survey (2022)
# File: import.r
# Last updated: July 25, 2022
# Author: Mari Roberts

# Load data directly from google sheets
############################################

# load packages
library(googlesheets4)

# # Use thi code to connect to gdrive, fetch new token, and authorize tidyverse api
# # make sure you select the option to be able edit all sheets
# googlesheets4::gs4_deauth()
# googlesheets4::gs4_auth()

# read google sheets data into r
# each sheet is a state
alabama.xlsx        <- read_sheet('https://docs.google.com/spreadsheets/d/1ggMloM9Y5W4mkZeGIi020Ycvbvxbw9TS-mbI_H0c3cY/edit?usp=sharing')
# alaska.xlsx          <- read_sheet('')
# arizona.xlsx         <- read_sheet('')
# arkansas.xlsx        <- read_sheet('')
# california.xlsx      <- read_sheet('')
# colorado.xlsx        <- read_sheet('')
# connecticut.xlsx     <- read_sheet('')
# delaware       <- read_sheet('')
# florida        <- read_sheet('')
# georgia        <- read_sheet('')
# hawaii         <- read_sheet('')
# idaho          <- read_sheet('')
# illinois       <- read_sheet('')
# indiana        <- read_sheet('')
# iowa           <- read_sheet('')
# kansas         <- read_sheet('')
# kentucky       <- read_sheet('')
# louisiana      <- read_sheet('')
# maine          <- read_sheet('')
# maryland       <- read_sheet('')
# massachusetts  <- read_sheet('')
# michigan       <- read_sheet('')
# minnesota      <- read_sheet('')
# mississippi    <- read_sheet('')
# missouri       <- read_sheet('')
# montana        <- read_sheet('')
# nebraska       <- read_sheet('')
# nevada         <- read_sheet('')
# new_hampshire  <- read_sheet('')
# new_jersey     <- read_sheet('')
# new_mexico     <- read_sheet('')
# new_york       <- read_sheet('')
# north_carolina <- read_sheet('')
# north_dakota   <- read_sheet('')
# ohio           <- read_sheet('')
# oklahoma       <- read_sheet('')
# oregon         <- read_sheet('')
# pennsylvania   <- read_sheet('')
# rhode_island   <- read_sheet('')
# south_carolina <- read_sheet('')
# south_dakota   <- read_sheet('')
# tennessee      <- read_sheet('')
# texas          <- read_sheet('')
# utah           <- read_sheet('')
# vermont        <- read_sheet('')
# virginia       <- read_sheet('')
# washington     <- read_sheet('')
# west_virginia  <- read_sheet('')
# wisconsin      <- read_sheet('')
# wyoming        <- read_sheet('')
