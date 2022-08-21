############################################
# Project:  MCLC Survey (2022)
# File: email.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Generate email depending on checklists created in generate.R
# Send emails

# Email structure

    # Thank you sentence
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

    "Hello, Thank you for participating in the 2022 More Community, Less Confinement data collection project. Please review your data submission below. Cells highlighted in green indicate new data that was submitted. Cells highlighted in yellow indicate that the field was left blank or requires attention.",
    "<br>",
    " ",
    form_button,
    " ",



    "## Data Quality Check",
    data_quality_sentence,

    qa_probation_violations,
    qa_probation_population_22,
    qa_parole_violations,
    qa_parole_population_22,

    "<br>",
    confirmed_definitions_sentence,
    definitions_table,
    "<br>",
    "<br>",
    "",

    "## Your Submission",
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




    "If you have any questions or concerns. Please reply to this email.",
    "",
    "<br>",
    "<br>",
    "Best,",
    "<br>",
    "Mari Roberts",
    "<br>"
  )),
  footer = ("The Council of State Governments Justice Center")
)

mclc_email

