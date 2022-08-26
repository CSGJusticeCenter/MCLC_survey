############################################
# Project:  MCLC Survey (2022)
# File: send_email.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Generate email and send to contact

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

# loop through states and generate email for each
for(i in 1:length(states)){

  # assign state name
  state_name <- states[i]

  # get contact info depending on state
  contact_info <- contact_list %>% filter(state == state_name)
  contact_info <- contact_info$email

  # custom function that creates email
  mclc_email <- fnc_email(adm_table_checklist, pop_table_checklist, costs_table_checklist, definitions_table_checklist, state_name)

  # save emails to view later
  assign(paste("email_", state_name,sep=''),mclc_email)

  # connect to outlook
  outlb <- get_business_outlook()

  # create email
  mclc_email <- outlb$create_email(mclc_email, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = contact_info)

  # send email
  mclc_email$send()
}

################
# View emails created/sent
################

email_Alabama
email_Idaho
email_Iowa
email_Pennsylvania
