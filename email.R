############################################
# Project:  MCLC Survey (2022)
# File: email.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Generate email depending on checklists created in generate.R
# Send emails
############################################


library(blastula)      #for creating emails with HTML
library(Microsoft365R) #for sending emails
library(webshot)       #for converting html output (markdown) to a picture format (png)
library(shiny)         #to write HTML code for custom buttons (blastula package is limited to a single html format)
library(tidyverse)

Lantern_PA <- "C:/Users/atallaksen/OneDrive - The Council of State Governments/GitProjects/LanternPA/Recommitment Reports/"

# I don't actually remember how I did this for the first time (once registered, you can just run this command again and again),
# but it is done through the Microsoft365R package
outlb <- get_business_outlook()

############################################################################################################################
########## Enter new dates etc., every month ###################################################################
############################################################################################################################

# CHANGE MM/YYYY FOR EMAIL SUBJECT LINES:
subject_line_text <- "Your Submission to the MCLC Survey (2022)"

############################################################################################################################
############################################################################################################################
############################################################################################################################

#create button to link to Cerberus
mybutton <-
  HTML('<table align="center">
       <tr>
           <td style="background-color:#446e9b; border-radius:10px; padding:10px; border: 1px solid #36587c;
                      transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
                      margin:0.5rem; text-shadow: -1px -1px 0 rgba(0, 0, 0, 0.1); box-sizing: border-box">
              <a style="color:white; text-decoration:none; font-family:tahoma; font-size:1rem; font-weight:400; line-height:1.5" href="https://csgjusticecenter.org/"><strong>Test Button</strong></a>
           </td>
       </tr>
       </table>')

############################################################################################################
# Create pngs
#############################################################################################################

(
  fig_1 <- mtcars %>%
    ggplot(aes(x = wt, y = mpg)) + geom_point()
)
ggsave("fig_1.png", path = Lantern_PA, width = 5, height=6, bg = "transparent")

###################################################################################################################
# Create email
####################################################################################################################

test_email <- compose_email(
  body = md(glue::glue(

    "Hello,

      This is a test email:

      {mybutton}
      {paste0(add_image(file = paste0(Lantern_PA, 'fig_1.png'),width=500))}

      That's it")),

  footer = glue::glue("The Council of State Governments Justice Center."))

################################################################################################################################
# inspect emails
################################################################################################################################

# check emails
test_email

#######################################################################################################################
# add recipients to each email
#######################################################################################################################

# this is where the subject_line_text variable, which we created above, is useful
test_email <- outlb$create_email(test_email, content_type = "html")$set_subject(paste(subject_line_text))$set_recipients(to = c("mroberts@csg.org"))

################################################################################################################################
# send emails
###############################################################################################################################

test_email$send()
