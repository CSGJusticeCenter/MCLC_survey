############################################
# Project:  MCLC Survey (2022)
# File: send_email.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Send email
############################################

source("code/automated_emails/00_import.R")
source("code/automated_emails/01_functions.R")
source("code/automated_emails/02_functions_gt.R")
source("code/automated_emails/03_function_email.R")
source("code/automated_emails/04_previous_survey.R")
source("code/automated_emails/05_checklists.R")
source("code/automated_emails/06_create_email.R")

###################    WARNING    #####################

#  The following code will send an email to the state contact
#  So, make sure everything is accurate first

###################    WARNING    #####################

# connect to outlook
outlb <- get_business_outlook()

########
# First email to be sent out on September 14, 2022
# Second email to be sent out on September 21, 2022
########

# loop through states and send email to contact
for(i in 1:length(states)){
  # assign state name
  state_name <- states[i]

  # # get contact info depending on state
  # contact_info <- contact_list %>% filter(state == state_name)
  # contact_info <- contact_info$email

  # connect to outlook
  outlb <- get_business_outlook()

  state_email_name <- get(paste0("email_", state_name,sep=''))

  # create email
  # the commented out line will send to mari instead of contact
  state_email <- outlb$create_email(state_email_name, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = "mroberts@csg.org")
  # state_email <- outlb$create_email(state_email_name, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = contact_info, cc = c("jmallett@csg.org", "agunter@csg.org", "mroberts@csg.org"))

  # send email
  state_email$send()
}
