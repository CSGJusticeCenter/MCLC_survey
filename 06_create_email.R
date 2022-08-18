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
library(reactable)
library(glue)
library(gt)
library(gtExtras)

# connect to outlook
outlb <- get_business_outlook()

# email subject line
subject_line_text <- "More Community, Less Confinement Project (2022)"

#create button to link to form
form_button <-
  HTML('<table align="center">
       <tr>
           <td style="background-color:#446e9b; border-radius:10px; padding:10px; border: 1px solid #36587c;
                      transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
                      margin:0.5rem; text-shadow: -1px -1px 0 rgba(0, 0, 0, 0.1); box-sizing: border-box">
              <a style="color:white; text-decoration:none; font-family:tahoma; font-size:1rem; font-weight:400; line-height:1.5" href="https://csgjusticecenter.org/"><strong>Test Button</strong></a>
           </td>
       </tr>
       </table>')

###################################################################################################################
# Create email
####################################################################################################################

# set survey depenging on state
# Alabama for now
form <- "[here](https://docs.google.com/spreadsheets/d/1xIPV2AyBKYKPrBfcqAstaMUjQCsrhbo337SbP44QGYM/edit?usp=sharing)"
mari_email <- "[mroberts@csg.org](mailto:mroberts@csg.org)"



# email
# md uses markdown text
email <- compose_email(
  body = md(c(

    "Hello, Thank you for participating in the 2022 More Community, Less Confinement data collection project.",
    "<br>",
    "<br>",
    "### Data Quality Check",

    state_submission_sentence,
    "<br>",
    "<br>",
    "<br>",


    "### Your Submission",
    "Legend: Green = data is new; Yellow = left blank",


    adm_table,
    "<br>",

    pop_table,
    "<br>"

  )),
  footer = glue::glue("The Council of State Governments Justice Center")
)
email

