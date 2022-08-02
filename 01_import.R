############################################
# Project:  MCLC Survey (2022)
# File: import.r
# Last updated: July 25, 2022
# Author: Mari Roberts

# Load data directly from google sheets
############################################

# load packages
library(googlesheets4)

# # Use this code to connect to google drive, fetch new token, and authorize tidyverse api
# # Make sure the sheets have been officially shared with you
# googlesheets4::gs4_deauth()
# googlesheets4::gs4_auth()

# read google sheets data into r
# each sheet is a state
Alabama          <- read_sheet('https://docs.google.com/spreadsheets/d/1ggMloM9Y5W4mkZeGIi020Ycvbvxbw9TS-mbI_H0c3cY/edit?usp=sharing')
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
Idaho            <- read_sheet('https://docs.google.com/spreadsheets/d/1KpmE0WJIbrBs9Uuc-CyF77cJ8nETWHM4k0LYW0VPlPk/edit?usp=sharing')
# illinois       <- read_sheet('')
# indiana        <- read_sheet('')
Iowa             <- read_sheet('https://docs.google.com/spreadsheets/d/1AxedM8YOYems1YslCHQncHPiv8ENvIZnr3-dXJ6FzNY/edit?usp=sharing')
# kansas         <- read_sheet('')
# kentucky       <- read_sheet('')
# louisiana      <- read_sheet('')
# maine          <- read_sheet('')
# maryland       <- read_sheet('')
# massachusetts  <- read_sheet('')
# michigan       <- read_sheet('')
# minnesota      <- read_sheet('')
# mississippi    <- read_sheet('')
Missouri         <- read_sheet('https://docs.google.com/spreadsheets/d/1-aVOwXJvxqMNLNkrqbcX_cSF4S_T8u2PIl3L7rdk4Oc/edit?usp=sharing')
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
Pennsylvania     <- read_sheet('https://docs.google.com/spreadsheets/d/1FcMVgowc4fdYL-L_btFzUi-SgaA0Z3QTG2kxJncKOZo/edit?usp=sharing')
# rhode_island   <- read_sheet('')
# south_carolina <- read_sheet('')
# south_dakota   <- read_sheet('')
# tennessee      <- read_sheet('')
# texas # can't access google sheets
# utah           <- read_sheet('')
# vermont        <- read_sheet('')
# virginia       <- read_sheet('')
# washington     <- read_sheet('')
# west_virginia  <- read_sheet('')
# wisconsin      <- read_sheet('')
# wyoming        <- read_sheet('')
