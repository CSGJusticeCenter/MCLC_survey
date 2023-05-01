# MCLC Survey (2022)

This repository contains code that does two things:

1) Formats data for imputation.
2) Generates an email that is specific to each state. The email is comprised of:  

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
      |-- survey (run programs 00 thru 04 in this order; use for current MCLC data)
          |-- 00_library_functions.R   # Custom functions to extract 2022 survey data and load packages
          |-- 01_import.R              # Imports MCLC survey data from Google Sheets and BJS data
          |-- 02_format_data.R         # Save data to sp - no longer needed due to manual changes in data
          |-- 03_clean.R               # Format data like 2021 survey data, replace data with BJS numbers
          |-- 04_check_data_issues.R   # Checks data submissions to see if data is repeated across metrics
          |-- 05_multiple_imputation.R # Input missing values
              |-- sources the following programs in this order: 03_clean.R, special_missing.R, MImp1.R, MImp3.R, MImp3.R, Costs.R
      |-- automated_emails
          |-- 00_import.R              # Load packages and imports MCLC survey data from Google Sheets  
          |-- 01_functions.R           # Custom functions for data cleaning and qa checklists
          |-- 02_functions_gt.R        # Custom functions focused on gt tables attributes
          |-- 03_functions_email.R     # Custom function to generate full email
          |-- 04_previous_survey.R     # 2021 survey data for comparisions
          |-- 05_checklists.R          # Create qa checklists
          |-- 06_create_email.R        # Create each state email 
          |-- 07_send_email.R          # Sends email to contact    
          
# Google Sheet Links

- [All forms](https://drive.google.com/drive/folders/1I-TbzusuCd9yTDkRZKucPoppQ2ONKrCV?usp=sharing)   
