

adm_table <- fnc_gt_adm_table(adm_table_checklist, "Alabama")
pop_table <- fnc_gt_pop_table(pop_table_checklist, "Alabama")


# gt table for population
fnc_gt_pop_table <- function(df, state_name){

  # filter by state
  df <- costs_table_checklist %>%
    filter(state == "Alabama") %>%
    select(-c(state))
  df <- tibble(df)

  cost_table <- gt(df) %>%

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
              locations = cells_body(columns = c(previous_2018))) %>%
    tab_style(style = list(cell_borders(side = c("left"), color = "gray", weight = px(1))),
              locations = cells_body(columns = c(current_2018))) %>%

    # hide data check columns
    cols_hide(columns = c(check_2019_21_22, check_2020_21_22)) %>%

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

    # change color to green if there were updates to the data from 2018 - 2020, and new data for 2021
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(current_2018), rows = previous_2018 != current_2018)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2019, current_2019), rows = previous_2019 != current_2019)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = c(previous_2020, current_2020), rows = previous_2020 != current_2020)) %>%
    tab_style(style = list(cell_fill(color = "#cefad0"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 != "Left Blank")) %>%

    # change color to yellow if a field was left blank
    tab_style(style = list(cell_fill(color = "yellow"), cell_text(weight = "bold")),
              locations = cells_body(columns = current_2021, rows = current_2021 == "Left Blank")) %>%

    # change to raw html for email
    as_raw_html()

  return(pop_table)
}
