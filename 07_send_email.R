############################################
# Project:  MCLC Survey (2022)
# File: send_email.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Send email
############################################


###################    WARNING    #####################

#  The following code will send an email to the state contact
#  So, make sure everything is accurate first

#  For now, the emails are going to Mari

###################    WARNING    #####################


########
# First email to be sent out on September 12, 2022
########

# loop through states and send email to contact
for(i in 1:length(states)){
  # assign state name
  state_name <- states[i]

  # get contact info depending on state
  # contact info is mari for now so comments will be removed later
  # contact_info <- contact_list %>% filter(state == state_name)
  # contact_info <- contact_info$email

  # connect to outlook
  outlb <- get_business_outlook()

  state_email_name <- get(paste0("email_", state_name,sep=''))

  # create email
  # contact info is mari for now so comments will be removed later
  # state_email <- outlb$create_email(state_email_name, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = contact_info)
  state_email <- outlb$create_email(state_email_name, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = "mroberts@csg.org")

  # send email
  state_email$send()
}


########
# Second email to be sent out on September 12, 2022
########
