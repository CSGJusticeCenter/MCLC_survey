####################################################
# Final Email Function
####################################################

# custom function that generates a email of the following:
# 1) sentence about data submission
# 2) sentences about whether data doesn't add up correctly and whether data was left blank, if applicable
# 3) tables that show data that doesn't add up, if applicable
# 4) table that shows definitions that weren't confirmed
# 5) submission tables showing green cells as new data and yellow cells as "left blank"

fnc_email <- function(adm_df, pop_df, costs_df, definitions_df, state_name){

  ############
  # check for data issues and save as TRUE/FALSE
  ############

  # determine whether someone left data blank in the admissions section
  state_adm <- adm_table_checklist %>% filter(state ==  state_name)
  left_blank_adm <- any(state_adm=="Left Blank")

  # determine whether someone left data blank in the population section
  state_pop <- pop_table_checklist %>% filter(state ==  state_name)
  left_blank_pop <- any(state_pop=="Left Blank")

  # determine whether someone left data blank in the costs section
  state_costs <- costs_table_checklist %>% filter(state ==  state_name)
  left_blank_costs <- any(state_costs=="Left Blank")

  # determine whether someone left data blank in the confirm definitions section
  state_definitions <- definitions_table_checklist %>% filter(state ==  state_name)
  left_blank_definitions <- any(state_definitions$definition_confirmation=="Not Confirmed")

  # filter to state
  state_data_quality <- state_data_checklist %>% filter(state == state_name)

  # determine whether data doesn't add up correctly
  check_supervision_violation_admissions_22     <- any(state_data_quality$check_supervision_violation_admissions_22     =="Doesn't Add Up")
  check_probation_violation_admissions_22       <- any(state_data_quality$check_probation_violation_admissions_22       =="Doesn't Add Up")
  check_parole_violation_admissions_22          <- any(state_data_quality$check_parole_violation_admissions_22          =="Doesn't Add Up")
  check_new_offense_violation_admissions_22     <- any(state_data_quality$check_new_offense_violation_admissions_22     =="Doesn't Add Up")
  check_technical_violation_admissions_22       <- any(state_data_quality$check_total_technical_violation_admissions_22 =="Doesn't Add Up")

  check_supervision_violation_population_22     <- any(state_data_quality$check_supervision_violation_population_22     =="Doesn't Add Up")
  check_probation_violation_population_22       <- any(state_data_quality$check_probation_violation_population_22       =="Doesn't Add Up")
  check_parole_violation_population_22          <- any(state_data_quality$check_parole_violation_population_22          =="Doesn't Add Up")
  check_new_offense_violation_population_22     <- any(state_data_quality$check_new_offense_violation_population_22     =="Doesn't Add Up")
  check_technical_violation_population_22       <- any(state_data_quality$check_total_technical_violation_population_22 =="Doesn't Add Up")

  # make all checks a df and see if any checks found an issue
  checks <- c(check_supervision_violation_admissions_22, check_probation_violation_admissions_22, check_parole_violation_admissions_22, check_new_offense_violation_admissions_22, check_technical_violation_admissions_22,
              check_supervision_violation_population_22, check_probation_violation_population_22, check_parole_violation_population_22, check_new_offense_violation_population_22, check_technical_violation_population_22)
  checks <- as.data.frame(checks)
  checks <- any(checks$checks == TRUE)

  ############
  # sentences
  ############

  # generate sentence depending on data issues
  # if a sentence isn't needed, then <span></span> is input into the email. This is basically a blank space in html that doesn't mess up formatting
  data_quality_sentence <- case_when(

    left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == FALSE & checks == FALSE ~ "Thank you for submitting data! Please review your submission below. If you need to make any updates, please click on the button above.<br>",
    left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == TRUE  & checks == FALSE ~ "We noticed you left some fields in the Costs Previously Submitted section blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == FALSE & checks == FALSE ~ "We noticed you left some fields in the Admissions and Population sections blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == TRUE  & checks == FALSE ~ "We noticed you left some fields in the Population and Costs Previously Submitted sections blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == FALSE & left_blank_costs == FALSE & checks == FALSE ~ "We noticed you left some fields in the Admissions section blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == FALSE & left_blank_costs == TRUE  & checks == FALSE ~ "We noticed you left some fields in the Population section blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == TRUE  & left_blank_costs == TRUE  & checks == FALSE ~ "We noticed you left some fields in the Admissions, Population, and Costs Previously Submitted sections blank. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == TRUE  & left_blank_costs == FALSE & checks == FALSE ~ "We noticed you left some fields in the Admissions and Population sections blank. If you do not have data, please input NA in your form.<br>",

    left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == FALSE & checks == TRUE  ~ "We noticed some potential issues with your data. Please review the issue(s) in the table(s) below.<br>",
    left_blank_adm == FALSE & left_blank_pop == FALSE & left_blank_costs == TRUE  & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Costs Previously Submitted section blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == FALSE & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Population section blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == FALSE & left_blank_pop == TRUE  & left_blank_costs == TRUE  & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Population and Costs Previously Submitted sections blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == FALSE & left_blank_costs == FALSE & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Admissions section blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == FALSE & left_blank_costs == TRUE  & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Admissions and Costs Previously Submitted sections blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == TRUE  & left_blank_costs == FALSE & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Admissions and Population sections blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",
    left_blank_adm == TRUE  & left_blank_pop == TRUE  & left_blank_costs == TRUE  & checks == TRUE  ~ "We noticed some potential issues with your data and that you left some fields in the Admissions, Population, and Costs Previously Submitted sections blank. Please review the issue(s) in the table(s) below. If you do not have data, please input NA in your form.<br>",

    TRUE ~ "<span></span>"
  )

  # generate sentence depending on definition issues
  confirmed_definitions_sentence <- case_when(
    left_blank_definitions == TRUE ~ "You did not confirm the following definitions or input anything in the definition notes. Please go to your state form and confirm that these definitions are correct or let us know how your definitions differ.<br>",
    left_blank_definitions == FALSE ~ "<span></span>"
  )

  # add title to defitions section if needed
  definitions_title <- case_when(
    left_blank_definitions == TRUE ~ paste("","## Unconfirmed Definitions", "<br>"),
    left_blank_definitions == FALSE ~ "<span></span>"
  )

  # custom function to generate sentence about whether data doesn't add up. otherwise, leave blank
  qa_supervision_violations_adm <- fnc_qa_sentence_adm(check_supervision_violation_admissions_22, "supervision violation admissions", "probation violation admissions",             "parole violation admissions")
  qa_probation_violations_adm   <- fnc_qa_sentence_adm(check_probation_violation_admissions_22,   "probation violation admissions",   "technical probation violation admissions",   "new offense probation violation admissions")
  qa_parole_violations_adm      <- fnc_qa_sentence_adm(check_parole_violation_admissions_22,      "parole violation admissions",      "technical parole violation admissions",      "new offense parole violation admissions")
  qa_new_offense_violations_adm <- fnc_qa_sentence_adm(check_new_offense_violation_admissions_22, "new offense violation admissions", "new offense probation violation admissions", "new offense parole violation admissions")
  qa_technical_violations_adm   <- fnc_qa_sentence_adm(check_technical_violation_admissions_22,   "technical violation admissions",   "technical probation violation admissions",   "technical parole violation admissions")
  qa_supervision_violations_pop <- fnc_qa_sentence_pop(check_supervision_violation_population_22, "supervision violation population", "probation violation population",             "parole violation population")
  qa_probation_violations_pop   <- fnc_qa_sentence_pop(check_probation_violation_population_22,   "probation violation population",   "technical probation violation population",   "new offense probation violation population")
  qa_parole_violations_pop      <- fnc_qa_sentence_pop(check_parole_violation_population_22,      "parole violation population",      "technical parole violation population",      "new offense parole violation population")
  qa_new_offense_violations_pop <- fnc_qa_sentence_pop(check_new_offense_violation_population_22, "new offense violation population", "new offense probation violation population", "new offense parole violation population")
  qa_technical_violations_pop   <- fnc_qa_sentence_pop(check_technical_violation_population_22,   "technical violation population",   "technical probation violation population",   "technical parole violation population")

  ############
  # qa tables
  ############

  # create qa table for supervision violation admissions if it exists
  qa_supervision_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                            state =  state_name,
                                                            "probation_violation_admissions_22",
                                                            "parole_violation_admissions_22",
                                                            "total_supervision_violation_admissions_22",
                                                            "check_supervision_violation_admissions_22",
                                                            fnc_qa_supervision_adm_headers)
  {if(is.list(qa_supervision_violation_admissions_22)){
    qa_supervision_violation_admissions_22 <- qa_supervision_violation_admissions_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_supervision_violation_admissions_22), rows = check_supervision_violation_admissions_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_supervision_violation_admissions_22), rows = check_supervision_violation_admissions_22 == "Left Blank")) %>%

      as_raw_html()
  } else {
    qa_supervision_violation_admissions_22 <- "<span></span>"
  }
  }

  # create qa table for probation admissions if it exists
  qa_probation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                state =  state_name,
                                                "technical_probation_violation_admissions_22",
                                                "new_offense_probation_violation_admissions_22",
                                                "probation_violation_admissions_22",
                                                "check_probation_violation_admissions_22",
                                                fnc_qa_probation_adm_headers)
  {if(is.list(qa_probation_admissions_22)){
    qa_probation_admissions_22 <- qa_probation_admissions_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_probation_violation_admissions_22), rows = check_probation_violation_admissions_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_probation_violation_admissions_22), rows = check_probation_violation_admissions_22 == "Left Blank")) %>%
      as_raw_html()
  } else {qa_probation_admissions_22 <- "<span></span>"}}

  # create qa table for parole admissions if it exists
  qa_parole_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                             state =  state_name,
                                             "technical_parole_violation_admissions_22",
                                             "new_offense_parole_violation_admissions_22",
                                             "parole_violation_admissions_22",
                                             "check_parole_violation_admissions_22",
                                             fnc_qa_parole_adm_headers)
  {if(is.list(qa_parole_admissions_22)){
    qa_parole_admissions_22 <- qa_parole_admissions_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_parole_violation_admissions_22), rows = check_parole_violation_admissions_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_parole_violation_admissions_22), rows = check_parole_violation_admissions_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_parole_admissions_22 <- "<span></span>"
  }
  }

  # create qa table for supervision violation admissions if it exists
  qa_technical_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                          state =  state_name,
                                                          "technical_probation_violation_admissions_22",
                                                          "technical_parole_violation_admissions_22",
                                                          "total_technical_violation_admissions_22",
                                                          "check_total_technical_violation_admissions_22",
                                                          fnc_qa_technical_adm_headers)
  {if(is.list(qa_technical_violation_admissions_22)){
    qa_technical_violation_admissions_22 <- qa_technical_violation_admissions_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_total_technical_violation_admissions_22), rows = check_total_technical_violation_admissions_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_total_technical_violation_admissions_22), rows = check_total_technical_violation_admissions_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_technical_violation_admissions_22 <- "<span></span>"
  }
  }

  # create qa table for new offense violation admissions if it exists
  qa_new_offense_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                            state =  state_name,
                                                            "new_offense_probation_violation_admissions_22",
                                                            "new_offense_parole_violation_admissions_22",
                                                            "total_new_offense_admissions_22",
                                                            "check_new_offense_violation_admissions_22",
                                                            fnc_qa_new_offense_adm_headers)
  {if(is.list(qa_new_offense_violation_admissions_22)){
    qa_new_offense_violation_admissions_22 <- qa_new_offense_violation_admissions_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_new_offense_violation_admissions_22), rows = check_new_offense_violation_admissions_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_new_offense_violation_admissions_22), rows = check_new_offense_violation_admissions_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_new_offense_violation_admissions_22 <- "<span></span>"
  }
  }

  # create qa table for supervision violation population if it exists
  qa_supervision_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                            state =  state_name,
                                                            "probation_violation_population_22",
                                                            "parole_violation_population_22",
                                                            "total_supervision_violation_population_22",
                                                            "check_supervision_violation_population_22",
                                                            fnc_qa_supervision_pop_headers)
  {if(is.list(qa_supervision_violation_population_22)){
    qa_supervision_violation_population_22 <- qa_supervision_violation_population_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_supervision_violation_population_22), rows = check_supervision_violation_population_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_supervision_violation_population_22), rows = check_supervision_violation_population_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_supervision_violation_population_22 <- "<span></span>"
  }
  }

  # create qa table for probation population if it exists
  qa_probation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                state =  state_name,
                                                "technical_probation_violation_population_22",
                                                "new_offense_probation_violation_population_22",
                                                "probation_violation_population_22",
                                                "check_probation_violation_population_22",
                                                fnc_qa_probation_pop_headers)
  {if(is.list(qa_probation_population_22)){
    qa_probation_population_22 <- qa_probation_population_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_probation_violation_population_22), rows = check_probation_violation_population_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_probation_violation_population_22), rows = check_probation_violation_population_22 == "Left Blank")) %>%
      as_raw_html()
  } else {qa_probation_population_22 <- "<span></span>"}}

  # create qa table for parole population if it exists
  qa_parole_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                             state =  state_name,
                                             "technical_parole_violation_population_22",
                                             "new_offense_parole_violation_population_22",
                                             "parole_violation_population_22",
                                             "check_parole_violation_population_22",
                                             fnc_qa_parole_pop_headers)
  {if(is.list(qa_parole_population_22)){
    qa_parole_population_22 <- qa_parole_population_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_parole_violation_population_22), rows = check_parole_violation_population_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_parole_violation_population_22), rows = check_parole_violation_population_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_parole_population_22 <- "<span></span>"
  }
  }

  # create qa table for supervision violation population if it exists
  qa_technical_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                          state =  state_name,
                                                          "technical_probation_violation_population_22",
                                                          "technical_parole_violation_population_22",
                                                          "total_technical_violation_population_22",
                                                          "check_total_technical_violation_population_22",
                                                          fnc_qa_technical_pop_headers)
  {if(is.list(qa_technical_violation_population_22)){
    qa_technical_violation_population_22 <- qa_technical_violation_population_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_total_technical_violation_population_22), rows = check_total_technical_violation_population_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_total_technical_violation_population_22), rows = check_total_technical_violation_population_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_technical_violation_population_22 <- "<span></span>"
  }
  }

  # create qa table for new offense violation population if it exists
  qa_new_offense_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                            state =  state_name,
                                                            "new_offense_probation_violation_population_22",
                                                            "new_offense_parole_violation_population_22",
                                                            "total_new_offense_population_22",
                                                            "check_new_offense_violation_population_22",
                                                            fnc_qa_new_offense_pop_headers)
  {if(is.list(qa_new_offense_violation_population_22)){
    qa_new_offense_violation_population_22 <- qa_new_offense_violation_population_22 %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_new_offense_violation_population_22), rows = check_new_offense_violation_population_22 == "Doesn't Add Up")) %>%
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = c(check_new_offense_violation_population_22), rows = check_new_offense_violation_population_22 == "Left Blank")) %>%
      as_raw_html()
  } else {
    qa_new_offense_violation_population_22 <- "<span></span>"
  }
  }

  ############
  # gt tables
  ############

  # generate definition confirmation table for emails
  definitions_table <- fnc_gt_definitions_table(definitions_table_checklist, state_name)

  # generate admissions data tables for emails
  adm_table <- fnc_gt_adm_table(adm_table_checklist, state_name)

  # generate population data tables for emails
  pop_table <- fnc_gt_pop_table(pop_table_checklist, state_name)

  # generate costs tables for emails
  costs_table <- fnc_gt_costs_table(costs_table_checklist, state_name)

  # generate notes and comments tables for emails
  notes_comments_table <- fnc_gt_notes_comments_table(notes_comments_list, state_name)

  ############
  # email
  ############

  #create button to link to form
  LINK <- form_links %>% filter(state == state_name)
  LINK <- LINK$form_link
  form_button <- HTML(paste0('<table align="center"><tr><td style="background-color:#355DA1; border-radius:5px; padding:10px; border: 1px solid #355DA1;transition: color 0.15s ease-in-out, background-color 0.15s ease-in-out, border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;margin:0.5rem; text-shadow: -1px -1px 0 rgba(0, 0, 0, 0.1); box-sizing: border-box">
              <a style="color:white; text-decoration:none; font-size:1rem; font-weight:400; line-height:1.5" href="', LINK, '"><strong>Your State Form</strong></a></td></tr></table>'))

  mclc_website <- "[More Community, Less Confinement](https://csgjusticecenter.org/publications/more-community-less-confinement/national-report/)"

  # md uses markdown text
  mclc_email <- compose_email(
    body = md(c(

      paste("#", state_name),
      " ",
      "Hello, Thank you for participating in the 2022 ", mclc_website, " data collection project.",
      "<br><br>",
      "Please review your data submission below. Cells highlighted in green indicate new data that was submitted. Cells highlighted in yellow indicate that the field was left blank or requires attention.",
      "<br>",
      " ",
      form_button,
      " ",



      "## Data Quality Check",
      "<b>",data_quality_sentence,"</b>",
      qa_supervision_violations_adm,
      qa_supervision_violation_admissions_22,
      qa_probation_violations_adm,
      qa_probation_admissions_22,
      qa_parole_violations_adm,
      qa_parole_admissions_22,
      qa_technical_violations_adm,
      qa_technical_violation_admissions_22,
      qa_new_offense_violations_adm,
      qa_new_offense_violation_admissions_22,
      qa_supervision_violations_pop,
      qa_supervision_violation_population_22,
      qa_probation_violations_pop,
      qa_probation_population_22,
      qa_parole_violations_pop,
      qa_parole_population_22,
      qa_technical_violations_pop,
      qa_technical_violation_population_22,
      qa_new_offense_violations_pop,
      qa_new_offense_violation_population_22,


      "<br>",
      "",
      definitions_title,
      confirmed_definitions_sentence,
      definitions_table,
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

  return(mclc_email)

}
