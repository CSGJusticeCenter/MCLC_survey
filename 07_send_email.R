
# this is where the subject_line_text variable, which we created above, is useful
mclc_email <- outlb$create_email(mclc_email, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = c("mroberts@csg.org"))


################################################################################################################################
# send emails
###############################################################################################################################

mclc_email$send()
