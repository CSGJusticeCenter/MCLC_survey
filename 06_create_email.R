############################################
# Project:  MCLC Survey (2022)
# File: email.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate email

# Email structure

    # Thank you sentence
    # Data quality sentence
    # Button link to form

    # Data Quality Checks

        # If data doesn't add up
        # If definitions weren't confirmed

    # Your submission

        # Admissions
        # Population
        # Costs
        # Notes/Comments
############################################

# # view email for Iowa (test file with lots of errors)
# fnc_email(adm_table_checklist, pop_table_checklist, costs_table_checklist, definitions_table_checklist, "Iowa")

# loop through states and generate email for each
for(i in 1:length(states)){

  # assign state name
  state_name <- states[i]

  # custom function that creates email
  email <- fnc_email(adm_table_checklist, pop_table_checklist, costs_table_checklist, definitions_table_checklist, state_name)

  # save to hold results
  assign(paste("email_", state_name,sep=''),email)

}

################
# View emails created
################

email_Alabama
email_Idaho
email_Iowa
email_Pennsylvania
