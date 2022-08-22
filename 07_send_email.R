############################################
# Project:  MCLC Survey (2022)
# File: send_email.R
# Last updated: August 22, 2022
# Author: Mari Roberts

# Send email to respondents
############################################

# connect to outlook
outlb <- get_business_outlook()

# assign email depending on state
mclc_email <- email_Alabama

# this is where the subject_line_text variable, which we created above, is useful
mclc_email <- outlb$create_email(mclc_email, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = c("mroberts@csg.org"))

# sent email
mclc_email$send()
