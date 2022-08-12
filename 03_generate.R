############################################
# Project:  MCLC Survey (2022)
# File: generate.R
# Last updated: July 25, 2022
# Author: Mari Roberts

# Save state contact info
# Generate state checklists
# Checklist for data, definitions, costs, notes
############################################


formattable(temp3,
            list(metric = formatter("span", style = ~ style(color = "black",font.weight = "bold")),
                 previous_2018 = formatter("span", style = ~style(display = "block", padding = "0 4px", `border-radius` = "1px",
                                                                  `background-color`= case_when(current_2018 == "Same" ~ "white",
                                                                                                current_2018 == "Different" ~ "lightseagreen"))),
                 previous_2019 = formatter("span", style = ~style(display = "block", padding = "0 4px", `border-radius` = "1px",
                                                                  `background-color`= case_when(current_2019 == "Same" ~ "white",
                                                                                                current_2019 == "Different" ~ "lightseagreen"))),
                 previous_2020 = formatter("span", style = ~style(display = "block", padding = "0 4px", `border-radius` = "1px",
                                                                  `background-color`= case_when(current_2020 == "Same" ~ "white",
                                                                                                current_2020 == "Different" ~ "lightseagreen")))

            ))


# create list containing each state's submission in google sheets
dfs <- list(Alabama, Idaho, Iowa)
dfs <- setNames(dfs,c("Alabama", "Idaho", "Iowa"))

# create a vector state names
states <- c("Alabama", "Idaho", "Iowa")

# run custom function that creates a list of data checklists for each state
# for example, if data doesn't add up correctly or they didn't input data
# accounts for misspellings of "na"
# ignore the warning message, it's about changing some values to NA when-
# it's not possible to calculate something because of a missing data value
# change list into a data frame
state_data_checklist <- map(.x = states,  .f = function(x) {
  df_state <- dfs[[x]]
  df_final[x] <- fnc_create_state_data_checklist(df_state, x)
})
state_data_checklist <- bind_rows(state_data_checklist)

# merge with previous survey data to check for data changes for 2018-2020
# remove 2021 since we're comparing 2018-2020 between both 2021 and 2022 data collection
# remove variables not needed
previous_survey_checklist <- state_data_checklist %>%
  left_join(previous_survey, by = c("state", "year")) %>%
  filter(year != 2021) %>%
  select(state, year, everything()) %>%
  select(-c(check_other_prison_admissions_22:check_parole_violation_population_22))

# perform checks by seeinng if what was submitted last year is different from what was submitted this year for 2018-2020
# append 21_22 to variables so we know we are comparing 2021 survey to 2022 survey
previous_survey_checklist <- previous_survey_checklist %>%
  mutate(check_total_prison_admissions_21_22                    = case_when(total_prison_admissions_21                    == total_prison_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_supervision_violation_admissions_21_22     = case_when(total_supervision_violation_admissions_21     == total_supervision_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_probation_violation_admissions_21_22             = case_when(probation_violation_admissions_21             == probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),

         check_parole_violation_admissions_21_22                = case_when(parole_violation_admissions_21                == parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_technical_violation_admissions_21_22       = case_when(total_technical_violation_admissions_21       == total_technical_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_technical_probation_violation_admissions_21_22   = case_when(technical_probation_violation_admissions_21   == technical_probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_technical_parole_violation_admissions_21_22      = case_when(technical_parole_violation_admissions_21      == technical_parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_total_new_offense_admissions_21_22               = case_when(total_new_offense_admissions_21               == total_new_offense_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_probation_violation_admissions_21_22 = case_when(new_offense_probation_violation_admissions_21 == new_offense_probation_violation_admissions_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_parole_violation_admissions_21_22    = case_when(new_offense_parole_violation_admissions_21    == new_offense_parole_violation_admissions_22 ~ "Same", TRUE ~ "Different"),

         check_total_prison_population_21_22                    = case_when(total_prison_population_21                    == total_prison_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_supervision_violation_population_21_22     = case_when(total_supervision_violation_population_21     == total_supervision_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_probation_violation_population_21_22             = case_when(probation_violation_population_21             == probation_violation_population_22 ~ "Same", TRUE ~ "Different"),

         check_parole_violation_population_21_22                = case_when(parole_violation_population_21                == parole_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_technical_violation_population_21_22       = case_when(total_technical_violation_population_21       == total_technical_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_technical_probation_violation_population_21_22   = case_when(technical_probation_violation_population_21   == technical_probation_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_technical_parole_violation_population_21_22      = case_when(technical_parole_violation_population_21      == technical_parole_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_total_new_offense_population_21_22               = case_when(total_new_offense_population_21               == total_new_offense_population_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_probation_violation_population_21_22 = case_when(new_offense_probation_violation_population_21 == new_offense_probation_violation_population_22 ~ "Same", TRUE ~ "Different"),
         check_new_offense_parole_violation_population_21_22    = case_when(new_offense_parole_violation_population_21    == new_offense_parole_violation_population_22 ~ "Same", TRUE ~ "Different")
         )

# Admissions
# Previous checklist

alabama_adm_21 <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, total_prison_admissions_21:new_offense_parole_violation_admissions_21) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank or No Data", .)))

alabama_adm_21_22 <- previous_survey_checklist %>% filter(state == "Alabama") %>%
  select(year, check_total_prison_admissions_21_22:check_new_offense_parole_violation_admissions_21_22)

alabama_adm_22 <- state_data_checklist %>% filter(state == "Alabama") %>%
  select(year, total_prison_admissions_22:new_offense_parole_violation_admissions_22)
alabama_adm_22 <- reshape2::dcast(reshape2::melt(alabama_adm_22, id.vars = "year"), variable ~ year)
alabama_adm_22 <- alabama_adm_22 %>%
  clean_names() %>%
  rename(current_2018 = x2018,
         current_2019 = x2019,
         current_2020 = x2020,
         current_2021 = x2021,
         metric = variable) %>%
  mutate(metric = gsub("_22", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
  mutate(metric = str_to_title(metric))

alabama_adm_21 <- reshape2::dcast(reshape2::melt(alabama_adm_21, id.vars = "year"), variable ~ year)
alabama_adm_21 <- alabama_adm_21 %>%
  clean_names() %>%
  rename(previous_2018 = x2018,
         previous_2019 = x2019,
         previous_2020 = x2020) %>%
  filter(variable != "state") %>%
  rename(metric = variable) %>%
  mutate(previous_2018 = comma(previous_2018, digits = 0),
         previous_2019 = comma(previous_2019, digits = 0),
         previous_2020 = comma(previous_2020, digits = 0)) %>%
  mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
  mutate(metric = str_to_title(metric))

alabama_adm_21_22 <- reshape2::dcast(reshape2::melt(alabama_adm_21_22, id.vars = "year"), variable ~ year)
alabama_adm_21_22 <- alabama_adm_21_22 %>%
  clean_names() %>%
  rename(check_2018 = x2018,
         check_2019 = x2019,
         check_2020 = x2020) %>%
  filter(variable != "state") %>%
  rename(metric = variable) %>%
  mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
  mutate(metric = str_to_title(metric))

alabama_adm <- merge(alabama_adm_22, alabama_adm_21, by = "metric", all.x = TRUE, all.y = TRUE)
alabama_adm <- merge(alabama_adm, alabama_adm_21_22, by = "metric", all.x = TRUE, all.y = TRUE)

library(dplyr)
library(reactable)

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

reactable(
  alabama_adm,
  pagination = FALSE,
  compact = TRUE,
  borderless = FALSE,
  striped = FALSE,
  fullWidth = FALSE,

  # Add theme for the top border
  theme = reactableTheme(
    headerStyle = list(
      "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
      borderColor = "#555"
    )
  ),

  defaultColDef = colDef(align = "center", minWidth = 75),

  columns = list(
    current_2018 = colDef(name = "2018"),
    current_2019 = colDef(name = "2019"),
    current_2020 = colDef(name = "2020"),
    # current_2021 = colDef(name = "2021"),
    current_2021 = colDef(
      # style = function() {list(background = "yellow")},
      style = JS("{background: 'rgba(0, 0, 0, 0.03)'}")
    ),
    previous_2018 = colDef(name = "2018"),
    previous_2019 = colDef(name = "2019"),
    previous_2020 = colDef(name = "2020")
  ),
  columnGroups = list(
    colGroup(name = "2022 Survey", columns = c("current_2018", "current_2019", "current_2020", "current_2021")),
    colGroup(name = "2021 Survey", columns = c("previous_2018", "previous_2019", "previous_2020"))
  )
)
