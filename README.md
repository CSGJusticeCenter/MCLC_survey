# MCLC Survey (2022)

This repository contains code that does four things:

1) Formats data for imputation.
2) Produces national estimates and state-by-state costs.
3) Produces rates and Relative Rate Indices RRIs for racial and ethnic disparities.
4) Generates an email that is specific to each state. The email is comprised of:  

    - Thank you sentence  
    - Button link to form  
    - Data submission quality sentence  
    - Data submission issues (sentence and corresponding table that shows how data doesn't add up), if applicable  
    - Definition issues, if applicable  
    - Submission summary (admissions, population, costs, notes, comments)  

## Data

This repository uses MCLC data from state specific Google Sheets. Version 4 had manual edits in Excel and replaced total admissions and total population with BJS numbers. Therefore, the most recent version of the data is version 5.  

## Repository Structure 

    |-- code 
      |-- survey
          |-- 00_library_functions.R   # Custom functions to extract 2022 survey data and load packages
          |-- 01_import.R              # Imports MCLC survey data from Google Sheets and BJS data
          |-- 02_format_data.R         # Save data to sp - no longer needed due to manual changes in data
          |-- 03_clean.R               # Format data like 2021 survey data, replace data with BJS numbers
          |-- 04_check_data_issues.R   # Checks data submissions to see if data is repeated across metrics
          |-- execute_report.R         # create final results for national report - executes 05_multiple_imputation.R
          |-- 05_multiple_imputation.R 
              |-- sources the following programs in this order: 03_clean.R, special_missings.R, MImp1.R, MImp3.R, MImp3.R, Costs.R
      |-- rates
          |-- ROOT.R                   # set root folder path
          |-- admin.R                  # prep for calculating rates
          |-- calc.R                   # runs clean_*.R programs to calculate rates and RRIs
          |-- clean_APS.R              # Annual Probation Survey data cleaning
          |-- clean_NCRP.R             # National Corrections Reporting Program data cleaning
          |-- clean_PUMS.R             # Census Public Use Microdata Sample data cleaning
          |-- clean_SC.R               # Census Annual Resident Population Estimates for 6 Race Groups data cleaning
          |-- import.R                 # import public data
      |-- automated_emails
          |-- 00_import.R              # Load packages and imports MCLC survey data from Google Sheets  
          |-- 01_functions.R           # Custom functions for data cleaning and qa checklists
          |-- 02_functions_gt.R        # Custom functions focused on gt tables attributes
          |-- 03_functions_email.R     # Custom function to generate full email
          |-- 04_previous_survey.R     # 2021 survey data for comparisions
          |-- 05_checklists.R          # Create qa checklists
          |-- 06_create_email.R        # Create each state email 
          |-- 07_send_email.R          # Sends email to contact  

## INSTRUCTIONS

In order to successfully produce national estimates and state-by-state costs, survey programs should be run in the following order: 

1) 00_library_functions.R 
2) 01_import.R
3) 02_format_data.R
4) execute_report.R

    - execute_report.R accepts parameters (set to a default for the current survey year) as well as for setting output file name and format.

5) 04_check_data_issues.R
          
# Google Sheet Links

- [All forms](https://drive.google.com/drive/folders/1I-TbzusuCd9yTDkRZKucPoppQ2ONKrCV?usp=sharing)   
