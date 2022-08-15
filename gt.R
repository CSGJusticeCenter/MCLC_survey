
df <- alabama_adm %>%
  select(-c(check_2018,check_2019,check_2020))
df <- tibble(alabama_adm)

# create_table <- function(data_df,
#                          fig_num = "",
#                          title_text = ""){
#
#   basic_table %>%
#     gt(data_df) %>%
#     tab_options(table.width = px(760),
#                 table.align = "left",
#                 heading.align = "left",
#                 # TODO: Discuss with Comms whether border should extend across
#                 # whole row at bottom or just across data cells
#                 table.border.top.style = "hidden",
#                 table.border.bottom.style = "transparent",
#                 heading.border.bottom.style = "hidden",
#                 # Need to set this to transparent so that cells_borders of the cells can display properly and
#                 table_body.border.bottom.style = "transparent",
#                 table_body.border.top.style = "transparent",
#                 # column_labels.border.bottom.style = "transparent",
#                 column_labels.border.bottom.width = px(1),
#                 column_labels.border.bottom.color = "gray",
#                 # row_group.border.top.style = "hidden",
#                 # Set font sizes
#                 # heading.title.font.size = px(14),
#                 # heading.subtitle.font.size = px(14),
#                 # column_labels.font.size = px(12),
#                 table.font.size = px(12),
#                 # source_notes.font.size = px(12),
#                 # footnotes.font.size = px(12),
#                 # Set row group label and border options
#                 # row_group.font.size = px(12),
#                 row_group.border.top.style = "transparent",
#                 row_group.border.bottom.style = "hidden",
#                 stub.border.style = "dashed"
#                 ) %>%
#
#     # table title and subtitle
#     tab_header(title = fig_num, subtitle = title_text) %>%
#
#     # subtitle
#     tab_style(
#       style = cell_text(color = "black", weight = "bold", align = "left"),
#       locations = cells_title("subtitle")) %>%
#
#     # title
#     tab_style(
#       style = cell_text(color = "#696969", weight = "normal", align = "left", transform = "uppercase"),
#       locations = cells_title("title")) %>%
#
#     # column headers
#     tab_style(
#       style = cell_text(color = "black", weight = "bold", size = px(12)),
#       locations = cells_column_labels(gt::everything())) %>%
#
#     # italicize row group and column spanner text
#     tab_style(
#       style = cell_text(color = "black", style = "italic", size  = px(12)),
#       locations = gt::cells_row_groups()) %>%
#
#     tab_style(
#       style = cell_text(color = "black", style = "italic", size  = px(12)),
#       locations = gt::cells_column_spanners()) %>%
#
#     # opt_table_font(font = list(google_font("Lato"),default_fonts())) %>%
#
#     # Adjust cell borders for all cells, small grey bottom border, no top border
#     tab_style(style = list(cell_borders(sides = c("bottom"), color = "#d2d2d2", weight = px(1))),
#       locations = list(cells_body(columns =  gt::everything()
#                                   # rows = gt::everything()
#                                   ))) %>%
#
#     tab_style(
#       style = list(
#         cell_borders(sides = c("top"), color = "#d2d2d2", weight = px(0))),
#       locations = list(cells_body(columns =  gt::everything()
#                                   # rows = gt::everything()
#                                   ))) %>%
#
#     # Set missing value defaults
#     fmt_missing(columns = gt::everything(), missing_text = "—")
#
#   return(basic_table)
# }

fnc_add_specific_customizations = function(gt_object){
  gt_object %>%
    cols_width(
      "metric"        ~ px(275),
      "previous_2018" ~ px(70),
      "previous_2018" ~ px(70),
      "previous_2018" ~ px(70),
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


adm_table_email <-

  gt(df) %>%

  # spanner for 2021 Survey
  tab_spanner(label = "Survey 2021", columns = c(previous_2018, previous_2019, previous_2020)) %>%
  tab_style(style = cell_text(size = px(12)),
            locations = cells_column_labels(columns = c(previous_2018, previous_2019, previous_2020))) %>%

  # spanner for 2022 survey
  tab_spanner(label = "Survey 2022", columns = c(current_2018, current_2019, current_2020, current_2021)) %>%
  tab_style(style = cell_text(size = px(12)),
            locations = cells_column_labels(columns = c(current_2018, current_2019, current_2020, current_2021))) %>%

  # table title and subtitle
  tab_header(title = "Prison Admissions", subtitle = "Data Quality Checks and Changes in Data from 2018 to 2020") %>%
  tab_style(style = cell_text(color = "black", weight = "bold", align = "left"),
            locations = cells_title("title")) %>%
  tab_style(style = cell_text(color = "#696969", weight = "normal", align = "left"),
            locations = cells_title("subtitle")) %>%

  # border lines around 2021 survey
  tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
            locations = cells_body(columns = c(previous_2018))) %>%
  tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
            locations = cells_body(columns = c(current_2018))) %>%

  # format columns with commas and no decimal places
  fmt_number(columns = 2:8, decimals = 0) %>%

  # hide data check columns
  cols_hide(columns = c(check_2018, check_2019, check_2020)) %>%

  # appearance settings
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
  ) %>%

  # specifications for column widths and labels
  fnc_add_specific_customizations() %>%

  # raw html for email
  as_raw_html()
