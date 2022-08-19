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
           <td style="background-color:#355DA1; border-radius:5px; padding:10px; border: 1px solid #355DA1;
                      transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
                      margin:0.5rem; text-shadow: -1px -1px 0 rgba(0, 0, 0, 0.1); box-sizing: border-box">
              <a style="color:white; text-decoration:none; font-size:1rem; font-weight:400; line-height:1.5" href="https://csgjusticecenter.org/"><strong>Your State Form</strong></a>
           </td>
       </tr>
       </table>')

# set survey depenging on state
# Alabama for now
form <- "[here](https://docs.google.com/spreadsheets/d/1xIPV2AyBKYKPrBfcqAstaMUjQCsrhbo337SbP44QGYM/edit?usp=sharing)"
mari_email <- "[mroberts@csg.org](mailto:mroberts@csg.org)"

# email
# md uses markdown text
mclc_email <- compose_email(
  body = md(c(

    "Hello, Thank you for participating in the 2022 More Community, Less Confinement data collection project.",
    "<br>",
    "<br>",
    "### Data Quality Check",

    submission_quality_sentence,
    "<br>",
    "<br>",

    " ",
    form_button,
    " ",

    "### Your Submission",
    "Cells highlighted in green indicate new data that was submitted. Cells highlighted in yellow indicate that the field was left blank and requires attention.",
    "<br>",
    adm_table,
    "<br>",

    pop_table,
    "<br>",

    costs_table,
    "<br>",

    notes_comments_table,
    "<br>",

    "Best,",
    "<br>",
    "Mari Roberts",
    "<br>"
  )),
  footer = ("The Council of State Governments Justice Center")
)
mclc_email

