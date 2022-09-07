############################################
# Project:  MCLC Survey (2022)
# File: create_email.R
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

# email subject line
subject_line_text <- "More Community, Less Confinement Project (2022)"

# break up loops into emails
# which states have submitted - dont email
# check matts code for csv

################
# Create email
################

# loop through states and generate email for each
# this code only generates the email, it does not send the email
# will take some time to generate all 50 states
for(i in 1:length(states)){
  # assign state name
  state_name <- states[i]

  # custom function that creates email
  mclc_email <- fnc_email(adm_table_checklist, pop_table_checklist, costs_table_checklist, definitions_table_checklist, state_name)

  # save emails to view and send
  assign(paste("email_", state_name,sep=''),mclc_email)
}

################
# View emails
################

  email_Alabama
  email_Alaska
  email_Arizona
  email_Arkansas
  email_California
  email_Colorado
  #email_Connecticut
  email_Delaware
  email_Florida
  email_Georgia
  email_Hawaii
  email_Idaho
  #email_Illinois
  email_Indiana
  email_Iowa
  email_Kansas
  email_Kentucky
  email_Louisiana
  email_Maine
  email_Maryland
  email_Massachusetts
  email_Michigan
  email_Minnesota
  email_Mississippi
  email_Missouri
  email_Montana
  email_Nebraska
  email_Nevada
 `email_New Hampshire`
 `email_New Jersey`
 `email_New Mexico`
 #`email_New York`
 `email_North Carolina`
 `email_North Dakota`
  email_Ohio
  email_Oklahoma
  email_Oregon
  email_Pennsylvania
 `email_Rhode Island`
  #`email_South Carolina` they asked for an extension so give them more time before emailing
 `email_South Dakota`
  email_Tennessee
  # email_Texas
  email_Utah
  email_Vermont
  email_Virginia
  email_Washington
 `email_West Virginia`
  email_Wisconsin
  email_Wyoming

