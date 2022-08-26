############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: August 22, 2022
# Author: Mari Roberts

# Custom functions to create gt tables for emails
############################################

#############################################
# GT Tables for Email
#############################################

# custom function to format table headers for admissions and population tables
fnc_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      "metric"        ~ px(270),
      "previous_2018" ~ px(70),
      "previous_2019" ~ px(70),
      "previous_2020" ~ px(70),
      "current_2018"  ~ px(70),
      "current_2019"  ~ px(70),
      "current_2020"  ~ px(70),
      "current_2021"  ~ px(70)
    ) %>%
    cols_label(
      metric        = "Data",
      previous_2018 = "2018",
      previous_2019 = "2019",
      previous_2020 = "2020",
      current_2018	= "2018",
      current_2019	= "2019",
      current_2020	= "2020",
      current_2021	= "2021"
    )
}

# custom function to format table headers for costs table
fnc_headers_costs <- function(gt_object){
  gt_object %>%
    cols_width(
      "previous_2019" ~ px(70),
      "previous_2020" ~ px(70),
      "current_2019"  ~ px(70),
      "current_2020"  ~ px(70),
      "current_2021"  ~ px(70)
    ) %>%
    cols_label(
      previous_2019 = "2019",
      previous_2020 = "2020",
      current_2019	= "2019",
      current_2020	= "2020",
      current_2021	= "2021"
    )
}

# custom function to format table for for all gt tables (spacing, font size, colors, etc.)
fnc_table_settings <- function(gt_object){
  gt_object %>%
    tab_options(#table.width = px(760),
      table.align = "left",
      heading.align = "left",

      # remove row at top
      table.border.top.style = "hidden",
      # table.border.bottom.style = "transparent",
      heading.border.bottom.style = "hidden",
      table.border.bottom.color = "gray",

      # need to set this to transparent so that cells_borders of the cells can display properly
      table_body.border.bottom.style = "transparent",
      table_body.border.top.style = "transparent",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "gray",

      # font sizes
      heading.title.font.size = px(14),
      heading.subtitle.font.size = px(12),
      column_labels.font.size = px(12),
      table.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.font.size = px(12),

      # row group label and border options
      row_group.font.size = px(12),
      row_group.border.top.style = "transparent",
      row_group.border.bottom.style = "hidden",
      stub.border.style = "dashed"
    )
}

####################################################################
# gt table qa checks
# if data adds up
####################################################################

# QA table for state and metric with data quality issues - if data doesn't add up
fnc_gt_qa_table <- function(df, state_name, variable_1, variable_2, variable_3, variable_4, header){
  # filter by state and metrics selected
  df1 <- state_data_checklist %>%
    dplyr::filter(state == state_name) %>%
    dplyr::select(-c(state)) %>%
    dplyr::mutate(plus = "+",
                  equal = "=") %>%
    dplyr::select(year,
                  variable_1,
                  plus,
                  variable_2,
                  equal,
                  variable_3,
                  variable_4)

  # if the data doesn't add up, output a table, otherwise, leave blank
  test <- any(df1=="Doesn't Add Up")

  { if(test == TRUE){
    qa_table <- gt(df1) %>%

      # # table title and subtitle
      # tab_header(title = "Data may not be accurate") %>%
      # tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
      #           locations = cells_title("title")) %>%

      # bold headers and year column
      tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(year))) %>%
      tab_style(locations = cells_column_labels(columns = everything()),
                style = list(cell_text(weight = "bold"))) %>%

      # add custom table settings and unique header (functions below) depending on metric
      fnc_table_settings() %>%
      header

  } else if(test == FALSE){
    qa_table <- "<span>"
  }
  }
}

# table headers for QA supervision violation admissions
fnc_qa_supervision_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_supervision_violation_admissions_22" ~ px(80),
      "probation_violation_admissions_22" ~ px(80),
      "parole_violation_admissions_22" ~ px(80),
      "check_supervision_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_supervision_violation_admissions_22  = "Total Supervision Violation admissions",
      probation_violation_admissions_22          = "Probation Violation admissions",
      parole_violation_admissions_22             = "Parole Violation admissions",
      check_supervision_violation_admissions_22  = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA probation admissions
fnc_qa_probation_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "probation_violation_admissions_22" ~ px(80),
      "new_offense_probation_violation_admissions_22" ~ px(80),
      "technical_probation_violation_admissions_22" ~ px(80),
      "check_probation_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      probation_violation_admissions_22             = "Probation Violation Admissions",
      new_offense_probation_violation_admissions_22 = "New Offense Probation Violation Admissions",
      technical_probation_violation_admissions_22   = "Technical Probation Violation Admissions",
      check_probation_violation_admissions_22       = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA parole admissions
fnc_qa_parole_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "parole_violation_admissions_22" ~ px(80),
      "new_offense_parole_violation_admissions_22" ~ px(80),
      "technical_parole_violation_admissions_22" ~ px(80),
      "check_parole_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      parole_violation_admissions_22             = "Parole Violation admissions",
      new_offense_parole_violation_admissions_22 = "New Offense Parole Violation admissions",
      technical_parole_violation_admissions_22   = "Technical Parole Violation admissions",
      check_parole_violation_admissions_22       = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA technical violation admissions
fnc_qa_technical_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_technical_violation_admissions_22" ~ px(80),
      "technical_probation_violation_admissions_22" ~ px(80),
      "technical_parole_violation_admissions_22" ~ px(80),
      "check_total_technical_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_technical_violation_admissions_22       = "Total Technical Violation Admissions",
      technical_probation_violation_admissions_22   = "Technical Probation Violation Admissions",
      technical_parole_violation_admissions_22      = "Technical Parole Violation Admissions",
      check_total_technical_violation_admissions_22 = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA new offense violation admissions
fnc_qa_new_offense_adm_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_new_offense_admissions_22" ~ px(80),
      "new_offense_probation_violation_admissions_22" ~ px(80),
      "new_offense_parole_violation_admissions_22" ~ px(80),
      "check_new_offense_violation_admissions_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_new_offense_admissions_22                 = "Total New Offense Violation admissions",
      new_offense_probation_violation_admissions_22   = "New Offense Probation Violation admissions",
      new_offense_parole_violation_admissions_22      = "New Offense Parole Violation admissions",
      check_new_offense_violation_admissions_22       = "Data Quality Check",
      plus                                            = " ",
      equal                                           = " ")
}

# table headers for QA supervision violation population
fnc_qa_supervision_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_supervision_violation_population_22" ~ px(80),
      "probation_violation_population_22" ~ px(80),
      "parole_violation_population_22" ~ px(80),
      "check_supervision_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_supervision_violation_population_22  = "Total Supervision Violation Population",
      probation_violation_population_22          = "Probation Violation Population",
      parole_violation_population_22             = "Parole Violation Population",
      check_supervision_violation_population_22  = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA probation population
fnc_qa_probation_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "probation_violation_population_22" ~ px(80),
      "new_offense_probation_violation_population_22" ~ px(80),
      "technical_probation_violation_population_22" ~ px(80),
      "check_probation_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      probation_violation_population_22             = "Probation Violation Population",
      new_offense_probation_violation_population_22 = "New Offense Probation Violation Population",
      technical_probation_violation_population_22   = "Technical Probation Violation Population",
      check_probation_violation_population_22       = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA parole population
fnc_qa_parole_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "parole_violation_population_22" ~ px(80),
      "new_offense_parole_violation_population_22" ~ px(80),
      "technical_parole_violation_population_22" ~ px(80),
      "check_parole_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      parole_violation_population_22             = "Parole Violation Population",
      new_offense_parole_violation_population_22 = "New Offense Parole Violation Population",
      technical_parole_violation_population_22   = "Technical Parole Violation Population",
      check_parole_violation_population_22       = "Data Quality Check",
      plus                                       = " ",
      equal                                      = " ")
}

# table headers for QA technical violation population
fnc_qa_technical_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_technical_violation_population_22" ~ px(80),
      "technical_probation_violation_population_22" ~ px(80),
      "technical_parole_violation_population_22" ~ px(80),
      "check_total_technical_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_technical_violation_population_22       = "Total Technical Violation Population",
      technical_probation_violation_population_22   = "Technical Probation Violation Population",
      technical_parole_violation_population_22      = "Technical Parole Violation Population",
      check_total_technical_violation_population_22 = "Data Quality Check",
      plus                                          = " ",
      equal                                         = " ")
}

# table headers for QA new offense violation population
fnc_qa_new_offense_pop_headers <- function(gt_object){
  gt_object %>%
    cols_width(
      year ~ px(50),
      "total_new_offense_population_22" ~ px(80),
      "new_offense_probation_violation_population_22" ~ px(80),
      "new_offense_parole_violation_population_22" ~ px(80),
      "check_new_offense_violation_population_22" ~ px(100)) %>%
    cols_label(
      year = "Year",
      total_new_offense_population_22                 = "Total New Offense Violation Population",
      new_offense_probation_violation_population_22   = "New Offense Probation Violation Population",
      new_offense_parole_violation_population_22      = "New Offense Parole Violation Population",
      check_new_offense_violation_population_22       = "Data Quality Check",
      plus                                            = " ",
      equal                                           = " ")
}

#####################################################################################
# gt table for definition confirmations
#####################################################################################

fnc_gt_definitions_table <- function(df, state_name){

  # filter by state and metrics selected
  df1 <- definitions_table_checklist %>%
    dplyr::filter(state == state_name) %>%
    dplyr::select(-c(state, definition_notes)) %>%
    filter(definition_confirmation == "Not Confirmed")

  { if(dim(df1)[1] != 0){
    definitions_table <- gt(df1) %>%

      # Set missing value defaults
      fmt_missing(columns = gt::everything(), missing_text = "") %>%

      # add custom table settings and unique header (functions below) depending on metric
      fnc_table_settings() %>%

      cols_width(
        "metric" ~ px(300),
        "include" ~ px(225),
        "dontinclude" ~ px(225),
        "definition_confirmation" ~  px(100)) %>%
      cols_label(
        metric = "Data",
        include = "Definitions",
        dontinclude = " ",
        definition_confirmation = "Confirm Definition") %>%

      # change color to yellow if a field was left blank
      tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                locations = cells_body(columns = definition_confirmation, rows = definition_confirmation == "Not Confirmed")) %>%

      tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
                locations = cells_body(columns = definition_confirmation, rows = definition_confirmation != "Not Confirmed")) %>%

      # change to raw html for email
      as_raw_html()

    return(definitions_table)

  } else if(dim(df1)[1] == 0){
    definitions_table <- "<span>"
  }
  }

}


#####################################################################################
# gt table for admissions
#####################################################################################

fnc_gt_adm_table <- function(df, state_name){

  # filter by state
  df <- adm_table_checklist %>%
    filter(state == "Alabama") %>%
    select(-c(state))
  df <- tibble(df)

  adm_table <- gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Prison Admissions", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(previous_2018))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2018))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2018_21_22, check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018 & (rows = current_2018 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019 & (rows = current_2019 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020 & (rows = current_2020 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2018, rows = current_2018 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(adm_table)
}

####################################################################
# gt table for population
####################################################################

fnc_gt_pop_table <- function(df, state_name){

  # filter by state
  df <- pop_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  pop_table <- gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Prison Population", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(previous_2018))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2018))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2018_21_22, check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers() %>%

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018 & (rows = current_2018 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019 & (rows = current_2019 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020 & (rows = current_2020 != "Left Blank"))) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2018, rows = current_2018 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(pop_table)
}

####################################################################
# gt table for costs
####################################################################

fnc_gt_costs_table <- function(df, state_name){

  # filter by state
  df <- costs_table_checklist %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  costs_table <- gt(df) %>%

    # spanner for 2021 Survey
    tab_spanner(label = "Survey 2021", columns = c(previous_2019, previous_2020)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(previous_2019, previous_2020))) %>%

    # spanner for 2022 survey
    tab_spanner(label = "Survey 2022", columns = c(current_2019, current_2020, current_2021)) %>%
    tab_style(style = cell_text(size = px(12)),
              locations = cells_column_labels(columns = c(current_2019, current_2020, current_2021))) %>%

    # table title and subtitle
    tab_header(title = "Cost Per Day Per Person", subtitle = "2021 Data and Changes in Data from 2018 to 2020") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%
    tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
              locations = cells_title("subtitle")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2019))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2019_21_22, check_2020_21_22)) %>%

    # appearance settings
    fnc_table_settings() %>%

    # specifications for column widths and labels
    fnc_headers_costs() %>%

    # change color to yellow if a field was left blank
    # change colors to green if data was changed between years
    # change colors to green if the data is new (2021)
    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2020), rows = current_2020 != "Left Blank" & previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2019, rows = current_2019 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2020, rows = current_2020 == "Left Blank")) %>%

    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(costs_table)
}

####################################################################
# gt table for notes and comments
####################################################################

fnc_gt_notes_comments_table <- function(df, state_name){

  # filter by state
  df <- notes_comments_list %>%
    clean_names() %>%
    filter(state == state_name) %>%
    select(-c(state))
  df <- tibble(df)

  notes_comments_table <- gt(df) %>%

    # table title and subtitle
    tab_header(title = "Notes and Comments") %>%
    tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
              locations = cells_title("title")) %>%

    # border lines around 2021 survey
    tab_style(style = list(cell_borders(side = c("right"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(notes))) %>%

    # appearance settings
    fnc_table_settings() %>%

    cols_width(
      "notes" ~ px(480),
      "comments" ~ px(280)) %>%
    cols_label(
      notes = "State Notes",
      comments = "Additional Comments") %>%

    # change to raw html for email
    as_raw_html()

  return(notes_comments_table)

}
