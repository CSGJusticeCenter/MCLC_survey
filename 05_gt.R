############################################
# Project:  MCLC Survey (2022)
# File: gt.R
# Last updated: August 10, 2022
# Author: Mari Roberts

# Generate tables for email
############################################

adm_table <- fnc_gt_adm_table(adm_table_checklist, "Pennsylvania")
pop_table <- fnc_gt_pop_table(pop_table_checklist, "Pennsylvania")
costs_table <- fnc_gt_costs_table(costs_table_checklist, "Pennsylvania")
notes_comments_table <- fnc_gt_notes_comments_table(notes_comments_list, "Pennsylvania")

# create qa table for parole population
qa_parole_population_22 <- fnc_gt_qa_table(df = state_data_quality,
                                           state = "Pennsylvania",
                                           "technical_parole_violation_population_22",
                                           "new_offense_parole_violation_population_22",
                                           "parole_violation_population_22",
                                           "check_parole_violation_population_22",
                                           fnc_qa_parole_pop_headers) %>%
                           # change color to red if data doesn't add up
                           tab_style(style = list(cell_fill(color = "red"), cell_text(weight = "bold")),
                                     locations = cells_body(columns = c(check_parole_violation_population_22), rows = check_parole_violation_population_22 == "Doesn't Add Up")) %>%
                           # change to raw html for email
                           as_raw_html()



