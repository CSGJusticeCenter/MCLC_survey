############################################
# Project:  MCLC Survey (2022)
# File: gt.R
# Last updated: August 10, 2022
# Author: Mari Roberts

# Generate gt tables for email
# Admissions, population, costs and notes/comments tables
# QA tables for supervision violations, probation, parole, technical, and new offense numbers that don't add up
############################################

##############
# qt tables
##############

# create qa table for supervision violation admissions if it exists
qa_supervision_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                           state = "Iowa",
                                           "probation_violation_admissions_22",
                                           "parole_violation_admissions_22",
                                           "total_supervision_violation_admissions_22",
                                           "check_supervision_violation_admissions_22",
                                           fnc_qa_supervision_adm_headers)
{if(is.list(qa_supervision_violation_admissions_22)){
  qa_supervision_violation_admissions_22 <- qa_supervision_violation_admissions_22 %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(check_supervision_violation_admissions_22), rows = check_supervision_violation_admissions_22 == "Doesn't Add Up")) %>%
    as_raw_html()
} else {
  qa_supervision_violation_admissions_22 <- ""
}
}

# create qa table for probation admissions if it exists
qa_probation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                              state = "Iowa",
                                              "technical_probation_violation_admissions_22",
                                              "new_offense_probation_violation_admissions_22",
                                              "probation_violation_admissions_22",
                                              "check_probation_violation_admissions_22",
                                              fnc_qa_probation_adm_headers)
    {if(is.list(qa_probation_admissions_22)){
      qa_probation_admissions_22 <- qa_probation_admissions_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_probation_violation_admissions_22), rows = check_probation_violation_admissions_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {qa_probation_admissions_22 <- ""}}

# create qa table for parole admissions if it exists
qa_parole_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                           state = "Iowa",
                                           "technical_parole_violation_admissions_22",
                                           "new_offense_parole_violation_admissions_22",
                                           "parole_violation_admissions_22",
                                           "check_parole_violation_admissions_22",
                                           fnc_qa_parole_adm_headers)
    {if(is.list(qa_parole_admissions_22)){
      qa_parole_admissions_22 <- qa_parole_admissions_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_parole_violation_admissions_22), rows = check_parole_violation_admissions_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_parole_admissions_22 <- ""
    }
}

# create qa table for supervision violation admissions if it exists
qa_technical_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                        state = "Iowa",
                                                        "technical_probation_violation_admissions_22",
                                                        "technical_parole_violation_admissions_22",
                                                        "total_technical_violation_admissions_22",
                                                        "check_total_technical_violation_admissions_22",
                                                        fnc_qa_technical_adm_headers)
{if(is.list(qa_technical_violation_admissions_22)){
  qa_technical_violation_admissions_22 <- qa_technical_violation_admissions_22 %>%
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(check_total_technical_violation_admissions_22), rows = check_total_technical_violation_admissions_22 == "Doesn't Add Up")) %>%
    as_raw_html()
} else {
  qa_technical_violation_admissions_22 <- ""
}
}

# create qa table for new offense violation admissions if it exists
qa_new_offense_violation_admissions_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                          state = "Iowa",
                                                          "new_offense_probation_violation_admissions_22",
                                                          "new_offense_parole_violation_admissions_22",
                                                          "total_new_offense_admissions_22",
                                                          "check_new_offense_violation_admissions_22",
                                                          fnc_qa_new_offense_adm_headers)
    {if(is.list(qa_new_offense_violation_admissions_22)){
      qa_new_offense_violation_admissions_22 <- qa_new_offense_violation_admissions_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_new_offense_violation_admissions_22), rows = check_new_offense_violation_admissions_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_new_offense_violation_admissions_22 <- ""
    }
    }

# create qa table for supervision violation population if it exists
qa_supervision_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                          state = "Iowa",
                                                          "probation_violation_population_22",
                                                          "parole_violation_population_22",
                                                          "total_supervision_violation_population_22",
                                                          "check_supervision_violation_population_22",
                                                          fnc_qa_supervision_pop_headers)
    {if(is.list(qa_supervision_violation_population_22)){
      qa_supervision_violation_population_22 <- qa_supervision_violation_population_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_supervision_violation_population_22), rows = check_supervision_violation_population_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_supervision_violation_population_22 <- ""
    }
    }

# create qa table for probation population if it exists
qa_probation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                              state = "Iowa",
                                              "technical_probation_violation_population_22",
                                              "new_offense_probation_violation_population_22",
                                              "probation_violation_population_22",
                                              "check_probation_violation_population_22",
                                              fnc_qa_probation_pop_headers)
    {if(is.list(qa_probation_population_22)){
      qa_probation_population_22 <- qa_probation_population_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_probation_violation_population_22), rows = check_probation_violation_population_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {qa_probation_population_22 <- ""}}

# create qa table for parole population if it exists
qa_parole_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                           state = "Iowa",
                                           "technical_parole_violation_population_22",
                                           "new_offense_parole_violation_population_22",
                                           "parole_violation_population_22",
                                           "check_parole_violation_population_22",
                                           fnc_qa_parole_pop_headers)
    {if(is.list(qa_parole_population_22)){
      qa_parole_population_22 <- qa_parole_population_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_parole_violation_population_22), rows = check_parole_violation_population_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_parole_population_22 <- ""
    }
    }

# create qa table for supervision violation population if it exists
qa_technical_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                        state = "Iowa",
                                                        "technical_probation_violation_population_22",
                                                        "technical_parole_violation_population_22",
                                                        "total_technical_violation_population_22",
                                                        "check_total_technical_violation_population_22",
                                                        fnc_qa_technical_pop_headers)
    {if(is.list(qa_technical_violation_population_22)){
      qa_technical_violation_population_22 <- qa_technical_violation_population_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_total_technical_violation_population_22), rows = check_total_technical_violation_population_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_technical_violation_population_22 <- ""
    }
    }

# create qa table for new offense violation population if it exists
qa_new_offense_violation_population_22 <- fnc_gt_qa_table(df = state_data_checklist,
                                                          state = "Iowa",
                                                          "new_offense_probation_violation_population_22",
                                                          "new_offense_parole_violation_population_22",
                                                          "total_new_offense_population_22",
                                                          "check_new_offense_violation_population_22",
                                                          fnc_qa_new_offense_pop_headers)
    {if(is.list(qa_new_offense_violation_population_22)){
      qa_new_offense_violation_population_22 <- qa_new_offense_violation_population_22 %>%
        tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
                  locations = cells_body(columns = c(check_new_offense_violation_population_22), rows = check_new_offense_violation_population_22 == "Doesn't Add Up")) %>%
        as_raw_html()
    } else {
      qa_new_offense_violation_population_22 <- ""
    }
    }




##############
# definition table
##############

# generate definition confirmation table for emails
definitions_table <- fnc_gt_definitions_table(definitions_table_checklist, "Iowa")

##############
# admissions and population table
##############

# generate admissions data tables for emails
adm_table <- fnc_gt_adm_table(adm_table_checklist, "Iowa")

# generate population data tables for emails
pop_table <- fnc_gt_pop_table(pop_table_checklist, "Iowa")

##############
# costs and notes
##############

# generate costs tables for emails
costs_table <- fnc_gt_costs_table(costs_table_checklist, "Iowa")

# generate notes and comments tables for emails
notes_comments_table <- fnc_gt_notes_comments_table(notes_comments_list, "Iowa")
