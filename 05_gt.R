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
