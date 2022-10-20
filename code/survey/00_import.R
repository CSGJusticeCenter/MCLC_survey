############################################
# Project:  MCLC Survey (2022)
# File: import.R
# Last updated:October 19, 2022
# Author: Mari Roberts

# Load data directly from google sheets
# Each state has it's own google sheet
# Previous survey file: Data for web team 2021 v13.xlsx
############################################

# Instructions for installing the csgjcr package
# In your Renviron (usethis::edit_r_environ(), set CSG_SP_PATH = "your sharepoint path here" and GITHUB_PAT = "your token here"
# remotes::install_github("csgjusticecenter/csgjcr")

# Load packages
library(csgjcr)
library(googlesheets4)
library(googledrive)
library(readxl)
library(janitor)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(formattable)
library(data.table)
library(blastula)
library(Microsoft365R)
library(webshot)
library(shiny)
library(tidyverse)
library(reactable)
library(glue)
library(gt)
library(gtExtras)
library(openxlsx)

#####
# 2021 survey
#####

# Get sharepoint path to get previous survey in 2021 folder.
sp_data_path <- csgjcr::csg_sp_path(file.path("JC Research - 50 State Revocations Project","50 State Survey (2021)", "Data"))

# Import survey data submitted in 2021 to compare with new submissions in 2022.
# This way we will know who changed their data for 2018, 2019, and 2020.
# Read in each admissions and population sheet from the xlsx.
adm18 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2018")
adm19 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2019")
adm20 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Admissions 2020")
pop18 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2018")
pop19 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2019")
pop20 <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Population 2020")

# Import costs.
costs <- read_excel(paste0(sp_data_path, "/Data for web team 2021 v13.xlsx", sep = ""), sheet = "Costs")

#####
# 2022 survey
#####

# Get sharepoint path to get Google form links.
# Will use these in the email.
sp_data_path <- csgjcr::csg_sp_path(file.path("JC Research - 50 State Revocations Project","50 State Survey (2022)"))
form_links <- read_excel(paste0(sp_data_path, "/MCLC 2022 Progress Tracking.xlsx", sep = ""))
form_links <- form_links %>% clean_names() %>% filter(!grepl('Excel', state)) %>%
  select(state, form_link = google_sheet_link_folder_https_drive_google_com_drive_folders_1i_tbzusu_cd9y_t_dk_rz_kuc_popp_q2on_kr_cv_usp_sharing)

###################    Attention    #####################

# Load each state google sheet one by one or there's a possibility the connection will time out.

# # Use this code to connect to google drive, fetch new token, and authorize tidyverse api
# # Make sure the sheets have been officially shared with you and you have editing access
# googlesheets4::gs4_deauth()
# googlesheets4::gs4_auth()

###################    Attention    #####################

# Import google sheets data
Alabama        <- read_sheet('https://docs.google.com/spreadsheets/d/1ggMloM9Y5W4mkZeGIi020Ycvbvxbw9TS-mbI_H0c3cY/edit?usp=sharing')
Alaska         <- read_sheet('https://docs.google.com/spreadsheets/d/1UY-tXd3E8zLWyh5H5T9vYW1YHiFIkM2PQDvlp_qVSjA/edit?usp=sharing')
Arizona        <- read_sheet('https://docs.google.com/spreadsheets/d/1HWxROOEmdYROs-V6bdMXt04OyG8NddRBfvx50QhZ-P0/edit?usp=sharing')
Arkansas       <- read_sheet('https://docs.google.com/spreadsheets/d/16e0WRzAWW7kh_Na_tf_8jbcdjM_do_7lpzI_P-eP8hU/edit?usp=sharing')
California     <- read_sheet('https://docs.google.com/spreadsheets/d/1dMD28LifJhlU85W8AYbTRE4V95p4SieXRwrCyLUZrXo/edit?usp=sharing')
Colorado       <- read_sheet('https://docs.google.com/spreadsheets/d/1pxOPsE-GCIhazJpwCZkpeXPY4VHdFwcFjEROXt5c4wU/edit?usp=sharing')
Connecticut    <- read_sheet('https://docs.google.com/spreadsheets/d/1E4OPFS3dkj3AK6vwuDI5btBJEBWNpobRMPnqIJf3jsw/edit?usp=sharing')
Delaware       <- read_sheet('https://docs.google.com/spreadsheets/d/111pvJGJl37BnSowV1vv0UHgttFlPKG2dNxn7dN-wERg/edit?usp=sharing')
Florida        <- read_sheet('https://docs.google.com/spreadsheets/d/14_fE47eIhGXDzXm52B8UnrFMY4JWlhqMhmO13zQqlCk/edit?usp=sharing')
Georgia        <- read_sheet('https://docs.google.com/spreadsheets/d/19xGMo2hiajuiQ9IJcPPItI4Md9ZKWTzI1hA3DH18Vqk/edit?usp=sharing')
Hawaii         <- read_sheet('https://docs.google.com/spreadsheets/d/1BKT-slHFfFq_Dat-zePTjh-t_8xZwwCfusNv61m0VTk/edit?usp=sharing')
Idaho          <- read_sheet('https://docs.google.com/spreadsheets/d/1KpmE0WJIbrBs9Uuc-CyF77cJ8nETWHM4k0LYW0VPlPk/edit?usp=sharing')
Illinois       <- read_sheet('https://docs.google.com/spreadsheets/d/1DXVve69iDWCZqqKvJ8MEzTiy2QMeQP62u9s1TykPtTU/edit?usp=sharing')
Indiana        <- read_sheet('https://docs.google.com/spreadsheets/d/1KGzRj3yM3UsAWfdLlJ50y0C-F0Me53HG2Epf0MWJEbM/edit?usp=sharing')
Iowa           <- read_sheet('https://docs.google.com/spreadsheets/d/1AxedM8YOYems1YslCHQncHPiv8ENvIZnr3-dXJ6FzNY/edit?usp=sharing')
Kansas         <- read_sheet('https://docs.google.com/spreadsheets/d/1TNpynCJgnkY_CrKVipsaLwPIxA3c4MZRg4YE3MZ0A4w/edit?usp=sharing')
Kentucky       <- read_sheet('https://docs.google.com/spreadsheets/d/1WH_-IkqxbHnyPh4kiGQCG3ZfG-b3HMHpJ5XQx0jSzFs/edit?usp=sharing')
Louisiana      <- read_sheet('https://docs.google.com/spreadsheets/d/1WTpaI1TNFrbRFUs7yR9WTVWu4tpUzpojiFlOwrSnN1E/edit?usp=sharing')
Maine          <- read_sheet('https://docs.google.com/spreadsheets/d/1WoNant7yxXdddzdP7kZmFl5IKWhAfiCT2dqxDJjbvwQ/edit?usp=sharing')
Maryland       <- read_sheet('https://docs.google.com/spreadsheets/d/1Yt6AOwdHmt39DehgOIIGhxO6C47wD1MNBXWTocg97Ko/edit?usp=sharing')
Massachusetts  <- read_sheet('https://docs.google.com/spreadsheets/d/1_WaR88UWCWCXVA6zi33kBv0x9KeHQ8TY-YTqLImFP8A/edit?usp=sharing')
Michigan       <- read_sheet('https://docs.google.com/spreadsheets/d/1_WmaixdktNTnHhB1UNEE_jDballq52gS2D59KQ3npE8/edit?usp=sharing')
Minnesota      <- read_sheet('https://docs.google.com/spreadsheets/d/1fqAHPBtWS1JIygwmB2UvoK3dLofmb1F_bkY9IV2WeW8/edit?usp=sharing')
Mississippi    <- read_sheet('https://docs.google.com/spreadsheets/d/1iZQlKGzA-Fo_3ZGTbfh2UqSYsJ7a2CmaPF5MpcRv0pg/edit?usp=sharing')
Missouri       <- read_sheet('https://docs.google.com/spreadsheets/d/1-aVOwXJvxqMNLNkrqbcX_cSF4S_T8u2PIl3L7rdk4Oc/edit?usp=sharing')
Montana        <- read_sheet('https://docs.google.com/spreadsheets/d/1jGVSQgStxa4XCj7mi7fwepmyqna2R2LfvM8iRVpFxXk/edit?usp=sharing')
Nebraska       <- read_sheet('https://docs.google.com/spreadsheets/d/1pBN5JwB5CXTKdJCvT84u3ZEz1J2hCRmCUrOOCApmPC0/edit?usp=sharing')
Nevada         <- read_sheet('https://docs.google.com/spreadsheets/d/1washpQiDySwzM6SrirSRj8AxZyDhqLUi-nzbaulUkXQ/edit?usp=sharing')
New_Hampshire  <- read_sheet('https://docs.google.com/spreadsheets/d/10UiIo5REsUfGopkW22vrK4aETVDrrAnF0tlxk2u6ykE/edit?usp=sharing')
New_Jersey     <- read_sheet('https://docs.google.com/spreadsheets/d/1C6xKjf5yWTTowXbbiKcrcCoL_eN3aptl7GYECBkoxY8/edit?usp=sharing')
New_Mexico     <- read_sheet('https://docs.google.com/spreadsheets/d/1CANUAioqFepSGNPxkG4ogYSYD568S-aIfvB_0uTnQw8/edit?usp=sharing')
New_York       <- read_sheet('https://docs.google.com/spreadsheets/d/1IrIFxk5t2cAEAY2UjYayw5O7FkvgoKZNrLfZFg7itMc/edit?usp=sharing')
North_Carolina <- read_sheet('https://docs.google.com/spreadsheets/d/1LlsTsiu6JUwEK-tFsdOhWiTdMe-9Y582dT-0ba4swkw/edit?usp=sharing')
North_Dakota   <- read_sheet('https://docs.google.com/spreadsheets/d/1NysXznACYJwzq9P8J5pDamcwQ7myPcscjnaDhI_Xg4g/edit?usp=sharing')
Ohio           <- read_sheet('https://docs.google.com/spreadsheets/d/1MylXsxfWaSVYefUJOdUTiFmH1g51xxS17moCCOxOiR8/edit?usp=sharing')
Oklahoma       <- read_sheet('https://docs.google.com/spreadsheets/d/1OpT0xpTYDgBpbmvoJXvQWGSg98NrUdgxBr8mql2hgu8/edit?usp=sharing')
Oregon         <- read_sheet('https://docs.google.com/spreadsheets/d/1Weg_lWBD8nl-89lKSwf9ZDa3Tr5IcYfJC__wpXG8m-A/edit?usp=sharing')
Pennsylvania   <- read_sheet('https://docs.google.com/spreadsheets/d/1FcMVgowc4fdYL-L_btFzUi-SgaA0Z3QTG2kxJncKOZo/edit?usp=sharing')
Rhode_Island   <- read_sheet('https://docs.google.com/spreadsheets/d/1fgA_0rdmEZW4MFIM02G7D_T_KwWFAB2y42qjlX7XlG4/edit?usp=sharing')
South_Carolina <- read_sheet('https://docs.google.com/spreadsheets/d/1gTBKIkbGBck8zbVMJ6-J2h6sNyWMs-t_fBg0At-uIZo/edit?usp=sharing')
South_Dakota   <- read_sheet('https://docs.google.com/spreadsheets/d/1hB1o7eY5kDl7XVZD7cjALxHMuzTnYEHwQC6wIEeN37g/edit?usp=sharing')
Tennessee      <- read_sheet('https://docs.google.com/spreadsheets/d/1i0GgBxzXVX6bAqylUkxBNSNsFih__ra9dvbfJuOGaMs/edit?usp=sharing')
Texas          <- read_sheet('https://docs.google.com/spreadsheets/d/1PprWIzpdkhG-7fKCkvmlvkogzY9Sd66AInV3_0VzffE/edit?usp=sharing')
Utah           <- read_sheet('https://docs.google.com/spreadsheets/d/1iIzx_z4f_flSvWfTtPVUy5qFh4AWfprSGBO58EUlu0g/edit?usp=sharing')
Vermont        <- read_sheet('https://docs.google.com/spreadsheets/d/1iMzx-RDAqE359x1dBqGzQttJnZoiQPZuMwwCFnL6N5g/edit?usp=sharing')
Virginia       <- read_sheet('https://docs.google.com/spreadsheets/d/15HhPmfEPysEJnoS7SPKGYQB0joHp7ZY_lWKRougeLyg/edit?usp=sharing')
Washington     <- read_sheet('https://docs.google.com/spreadsheets/d/1RWCYB0ZOfkeahWlKmrk71QibhL6pccphSTY1n5HzQJY/edit?usp=sharing')
West_Virginia  <- read_sheet('https://docs.google.com/spreadsheets/d/1SQYX-brKkDxx_PJJ4KrbwW4n63rwIh0KOtkIkdyi9TU/edit?usp=sharing')
Wisconsin      <- read_sheet('https://docs.google.com/spreadsheets/d/1dizewdEzbJ-oAMdtPio94IDrfSJF3NjMo_QHdjIl0n8/edit?usp=sharing')
Wyoming        <- read_sheet('https://docs.google.com/spreadsheets/d/1ealZ3x4blMav2KCaaseV5wXYN5nJWjYqhMQq_OAcpy0/edit?usp=sharing')

# Create list of dfs containing each state's submission in google sheets
state_dfs <- list(Alabama,
                  Alaska,
                  Arizona,
                  Arkansas,
                  California,
                  Colorado,
                  Connecticut,
                  Delaware,
                  Florida,
                  Georgia,
                  Hawaii,
                  Idaho,
                  Illinois,
                  Indiana,
                  Iowa,
                  Kansas,
                  Kentucky,
                  Louisiana,
                  Maine,
                  Maryland,
                  Massachusetts,
                  Michigan,
                  Minnesota,
                  Mississippi,
                  Missouri,
                  Montana,
                  Nebraska,
                  Nevada,
                  New_Hampshire,
                  New_Jersey,
                  New_Mexico,
                  New_York,
                  North_Carolina,
                  North_Dakota,
                  Ohio,
                  Oklahoma,
                  Oregon,
                  Pennsylvania,
                  Rhode_Island,
                  South_Carolina,
                  South_Dakota,
                  Tennessee,
                  Texas,
                  Utah,
                  Vermont,
                  Virginia,
                  Washington,
                  West_Virginia,
                  Wisconsin,
                  Wyoming)

# Set names
state_dfs <- setNames(state_dfs,c('Alabama',
                                  'Alaska',
                                  'Arizona',
                                  'Arkansas',
                                  'California',
                                  'Colorado',
                                  'Connecticut',
                                  'Delaware',
                                  'Florida',
                                  'Georgia',
                                  'Hawaii',
                                  'Idaho',
                                  'Illinois',
                                  'Indiana',
                                  'Iowa',
                                  'Kansas',
                                  'Kentucky',
                                  'Louisiana',
                                  'Maine',
                                  'Maryland',
                                  'Massachusetts',
                                  'Michigan',
                                  'Minnesota',
                                  'Mississippi',
                                  'Missouri',
                                  'Montana',
                                  'Nebraska',
                                  'Nevada',
                                  'New Hampshire',
                                  'New Jersey',
                                  'New Mexico',
                                  'New York',
                                  'North Carolina',
                                  'North Dakota',
                                  'Ohio',
                                  'Oklahoma',
                                  'Oregon',
                                  'Pennsylvania',
                                  'Rhode Island',
                                  'South Carolina',
                                  'South Dakota',
                                  'Tennessee',
                                  'Texas',
                                  'Utah',
                                  'Vermont',
                                  'Virginia',
                                  'Washington',
                                  'West Virginia',
                                  'Wisconsin',
                                  'Wyoming'))

# create a vector of state names for loops later
states <- c('Alabama',
            'Alaska',
            'Arizona',
            'Arkansas',
            'California',
            'Colorado',
            'Connecticut',
            'Delaware',
            'Florida',
            'Georgia',
            'Hawaii',
            'Idaho',
            'Illinois',
            'Indiana',
            'Iowa',
            'Kansas',
            'Kentucky',
            'Louisiana',
            'Maine',
            'Maryland',
            'Massachusetts',
            'Michigan',
            'Minnesota',
            'Mississippi',
            'Missouri',
            'Montana',
            'Nebraska',
            'Nevada',
            'New Hampshire',
            'New Jersey',
            'New Mexico',
            'New York',
            'North Carolina',
            'North Dakota',
            'Ohio',
            'Oklahoma',
            'Oregon',
            'Pennsylvania',
            'Rhode Island',
            'South Carolina',
            'South Dakota',
            'Tennessee',
            'Texas',
            'Utah',
            'Vermont',
            'Virginia',
            'Washington',
            'West Virginia',
            'Wisconsin',
            'Wyoming'
)
