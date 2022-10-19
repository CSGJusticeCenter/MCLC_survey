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

This repository uses MCLC data from state specific Google Sheets. 

## Repository Structure 

    |-- code (run in this order)
      |-- automated_emails
          |-- 00_import.R          # Load packages and imports MCLC survey data from Google Sheets  
          |-- 01_functions.R       # Custom functions for data cleaning and qa checklists
          |-- 02_functions_gt.R    # Custom functions focused on gt tables attributes
          |-- 03_functions_email.R # Custom function to generate full email
          |-- 04_previous_survey.R # 2021 survey data for comparisions
          |-- 05_checklists.R      # Create qa checklists
          |-- 06_create_email.R    # Create each state email 
          |-- 07_send_email.R      # Sends email to contact   
      |-- survey
          |-- 00_functions.R       # Custom functions to extract 2022 survey data 
          |-- 00_import.R          # Load packages and imports MCLC survey data from Google Sheets 
          |-- 01_format_data.R     # Format data like 2021 survey data
          |-- 02_impute.R          # Impute missing values

          
# Google Sheet Links

- [All forms](https://drive.google.com/drive/folders/1I-TbzusuCd9yTDkRZKucPoppQ2ONKrCV?usp=sharing)   
