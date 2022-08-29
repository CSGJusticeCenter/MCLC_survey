# MCLC Survey (2022)

For now, this code checks 4 test files (Alabama, Idaho, Iowa, Pennsylvania) that were created with errors.  

This repository contains code that generates an email that is specific to each state. The email is comprised of:  

- Thank you sentence  
- Button link to form  
- Data submission quality sentence  
- Data submission issues (sentence and corresponding table that shows how data doesn't add up), if applicable  
- Definition issues, if applicable  
- Submission summary (admissions, population, costs, notes, comments)  

## Data

This repository uses MCLC data from state specific Google Sheets. Connecticut, New York, and Texas will have excel sheets since they can't access Google Sheets. 

## Repository Structure 

    |-- code (run in this order)
      |-- 00_import.R          # load packages and imports MCLC survey data from Google Sheets  
      |-- 01_functions.R       # custom functions for data cleaning and qa checklists
      |-- 02_functions_gt.R    # custom functions focused on gt tables attributes
      |-- 03_functions_email.R # custom function to generate full email
      |-- 04_previous_survey.R # 2021 survey data for comparisions
      |-- 05_checklists.R      # Create qa checklists
      |-- 06_create_email.R    # Create each state email 
      |-- 07_send_email.R      # Sends email to contact   
# Google Sheet Links

- [All forms](https://drive.google.com/drive/folders/1I-TbzusuCd9yTDkRZKucPoppQ2ONKrCV?usp=sharing)   
- Connecticut, New York, and Texas will have excel sheets.  
