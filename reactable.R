# Table for R Markdown document

library(reactable)
library(htmltools)

forecasts <- read.csv("wwc_forecasts.csv", stringsAsFactors = FALSE)

rating_cols <- c("spi", "global_o", "global_d")
group_cols <- c("group_1", "group_2", "group_3")
knockout_cols <- c("make_round_of_16", "make_quarters", "make_semis", "make_final", "win_league")
forecasts <- forecasts[, c("team", "points", "group", rating_cols, group_cols, knockout_cols)]

rating_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
def_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 0.6)
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

tbl <- reactable(
  forecasts,
  pagination = FALSE,
  defaultSorted = "win_league",
  defaultSortOrder = "desc",
  defaultColGroup = colGroup(headerClass = "group-header"),
  columnGroups = list(
    colGroup(name = "Team Rating", columns = rating_cols),
    colGroup(name = "Chance of Finishing Group Stage In ...", columns = group_cols),
    colGroup(name = "Knockout Stage Chances", columns = knockout_cols)
  ),
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  columns = list(
    team = colDef(
      defaultSortOrder = "asc",
      minWidth = 200,
      headerStyle = list(fontWeight = 700),
      cell = function(value, index) {
        div(
          class = "team",
          img(class = "flag", alt = paste(value, "flag"), src = sprintf("images/%s.png", value)),
          div(class = "team-name", value),
          div(class = "record", sprintf("%s pts.", forecasts[index, "points"]))
        )
      }
    ),
    points = colDef(show = FALSE),
    group = colDef(defaultSortOrder = "asc", align = "center", maxWidth = 75,
                   class = "cell group", headerStyle = list(fontWeight = 700)),
    spi = rating_column(format = colFormat(digits = 1)),
    global_o = rating_column(
      name = "Off.",
      cell = function(value) {
        scaled <- (value - min(forecasts$global_o)) / (max(forecasts$global_o) - min(forecasts$global_o))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "spi-rating", style = list(background = color), value)
      }
    ),
    global_d = rating_column(
      name = "Def.",
      defaultSortOrder = "asc",
      cell = function(value) {
        scaled <- 1 - (value - min(forecasts$global_d)) / (max(forecasts$global_d) - min(forecasts$global_d))
        color <- def_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "spi-rating", style = list(background = color), value)
      }
    ),
    group_1 = group_column(name = "1st Place", class = "border-left"),
    group_2 = group_column(name = "2nd Place"),
    group_3 = group_column(name = "3rd Place"),
    make_round_of_16 = knockout_column(name = "Make Round of 16", class = "border-left"),
    make_quarters = knockout_column(name = "Make Qtr-Finals"),
    make_semis = knockout_column(name = "Make Semifinals", maxWidth = 90),
    make_final = knockout_column(name = "Make Final"),
    win_league = knockout_column(name = "Win World Cup")
  ),
  # Emphasize borders between groups when sorting by group
  rowClass = JS("
    function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'group') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"
  ),
  showSortIcon = FALSE,
  borderless = TRUE,
  class = "standings-table"
)

div(class = "standings",
    div(class = "title",
        h2("2019 Women's World Cup Predictions"),
        "Soccer Power Index (SPI) ratings and chances of advancing for every team"
    ),
    tbl,
    "Forecast from before 3rd group matches"
)


reactable(alabama_adm, columns = list(
  metric = colDef(
    style = function(value, index) {
      if (alabama_adm$metric[index] == "Total Prison Admissions" |
          alabama_adm$metric[index] == "Total New Offense Admissions" |
          alabama_adm$metric[index] == "Total Technical Violation Admissionss" |
          alabama_adm$metric[index] == "Total Supervision Violation Admissions") {
        fontWeight = 'bold'
      } else if (alabama_adm$metric[index] == "Probation Violation Admissions" |
                 alabama_adm$metric[index] == "Parole Violation Admissions" |
                 alabama_adm$metric[index] == "Technical Parole Violation Admissions" |
                 alabama_adm$metric[index] == "Technical Probation Violation Admissions" |
                 alabama_adm$metric[index] == "New Offense Probation Violation Admissions" |
                 alabama_adm$metric[index] == "New Offense Parole Violation Admissions") {
        fontWeight = 'regular'
      }
      list(fontWeight = fontWeight)
    }
  )
))

alabama_tbl <- reactable(
  alabama_adm,
  # theme = fivethirtyeight(),
  pagination = FALSE,
  compact = TRUE,
  borderless = FALSE,
  striped = FALSE,
  fullWidth = FALSE,

  defaultColDef = colDef(align = "center", minWidth = 75,
                         format = colFormat(separators = TRUE)),

  columns = list(
    metric = colDef(name = "Metric", width = 310,
                    align = "left",
                    style = function(value, index) {
                      if (alabama_adm$metric[index] == "Total Prison Admissions" |
                          alabama_adm$metric[index] == "Total New Offense Admissions" |
                          alabama_adm$metric[index] == "Total Technical Violation Admissions" |
                          alabama_adm$metric[index] == "Total Supervision Violation Admissions") {
                        fontWeight = 'bold'
                      } else if (alabama_adm$metric[index] == "Probation Violation Admissions" |
                                 alabama_adm$metric[index] == "Parole Violation Admissions" |
                                 alabama_adm$metric[index] == "Technical Parole Violation Admissions" |
                                 alabama_adm$metric[index] == "Technical Probation Violation Admissions" |
                                 alabama_adm$metric[index] == "New Offense Probation Violation Admissions" |
                                 alabama_adm$metric[index] == "New Offense Parole Violation Admissions") {
                        fontWeight = 'regular'
                      }
                      list(fontWeight = fontWeight)
                    }
    ),
    current_2018 = colDef(name = "2018",
                          style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")),
    current_2019 = colDef(name = "2019"),
    current_2020 = colDef(name = "2020"),
    current_2021 = colDef(name = "2021"),
    # current_2021 = colDef(
    #   # style = function() {list(background = "yellow")},
    #   style = JS("{background: 'rgba(0, 0, 0, 0.03)'}")
    # ),
    previous_2018 = colDef(name = "2018",
                           style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")),
    previous_2019 = colDef(name = "2019"),
    previous_2020 = colDef(name = "2020"),

    check_2018 = colDef(show = FALSE),
    check_2019 = colDef(show = FALSE),
    check_2020 = colDef(show = FALSE)
  ),
  columnGroups = list(
    colGroup(name = "2022 Survey", columns = c("current_2018", "current_2019", "current_2020", "current_2021")),
    colGroup(name = "2021 Survey", columns = c("previous_2018", "previous_2019", "previous_2020"))
  )
)
