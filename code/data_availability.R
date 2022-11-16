############################################
# Project:  MCLC Survey (2022)
# File: data_availability.R
# Last updated: November 16, 2022
# Author: Mari Roberts

# Tables showing what states submitted to MCLC from 2018 to 2021
# There was some manual coding in this file. Due to time constraints,
#    I could not figure out how to combine rows by state, year, and what they submitted, when they submitted
#    something different for some years. This was only done for Alaska, Maine, Maryland, Nebraska, New Hampshire,
#    New Jersey, and New Mexico
############################################

# load packages
library(csgjcr)
library(readxl)
library(dplyr)
library(tidyverse)
library(formattable)
library(htmltools)
library(webshot)
library(htmlwidgets)
library(extrafont)
library(sysfonts)

# # Add fonts
# font_add("Graphik Regular", regular = "GraphikRegular.otf")
# font_import(paths = "C:/Users/YOURNAME/AppData/Local/Microsoft/Windows/Fonts")
# extrafont::loadfonts()
# loadfonts(device="win")
# loadfonts(device="pdf")

my_color_bar <- function (color = "lightgray", fixedWidth=150,...)
{
  formatter("span", style = function(x) style(width = ))
}

yes_no_fmt  <- formattable::formatter(.tag = "span", style = function(x) style(color = ifelse(x == "No" , "#B05D24", "#5c9c80")), x ~ icontext(ifelse(x == "No", "glyphicon glyphicon-remove", "glyphicon glyphicon-ok"), x))
yes_no_fmt  <- formatter("span", style = function(x) style(display           = "inline-block",
                                                      direction         = "rtl",
                                                      `border-radius`   = "4px",
                                                      `padding-right`   = "2px",
                                                      `background-color`= "white",
                                                      width             = "80px",
                                                      color = ifelse(x == "No" , "#B05D24", "#5c9c80")), x ~ icontext(ifelse(x == "No", "glyphicon glyphicon-remove", "glyphicon glyphicon-ok"), x))

year_fmt <- formatter("span", style = function(x) style(display           = "inline-block",
                                                      `border-radius`   = "4px",
                                                      `padding-right`   = "2px",
                                                      `background-color`= "white",
                                                      width             = "95px"))

# Get sharepoint path to get previous survey in 2021 folder.
sp_data_path <- csgjcr::csg_sp_path(file.path("JC Research - 50 State Revocations Project","50 State Survey (2022)", "Data"))

# Which metrics did states submit?
adm18 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Admissions 2018")
adm19 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Admissions 2019")
adm20 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Admissions 2020")
adm21 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Admissions 2021")

pop18 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Population 2018")
pop19 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Population 2019")
pop20 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Population 2020")
pop21 <- read_excel(paste0(sp_data_path, "/mclc_data_2022_v2.xlsx", sep = ""), sheet = "Population 2021")

adm_all <- rbind(adm18,
             adm19,
             adm20,
             adm21)
pop_all <- rbind(pop18,
             pop19,
             pop20,
             pop21)

adm <- adm_all %>% mutate(
  across(.cols = total_prison_admissions:new_offense_parole_violation_admissions,
         .fns = ~ case_when(
          is.numeric(.x) & !is.na(.x) ~ 'Yes',
          is.numeric(.x) & is.na(.x)  ~ 'No'))) %>%
  #filter(year == 2021) %>%
  #filter_all(any_vars(grepl("No", .))) %>%
  select(`State` = state,
         `Year` = year,
         `Total Admissions` = total_prison_admissions,
         `Supervision Violation Admissions` = total_supervision_violation_admissions,
         `Probation Violation Admissions` = probation_violation_admissions,
         `Parole Violation Admissions` = parole_violation_admissions,
         `Technical Admissions` = total_technical_violation_admissions,
         `Probation Technical Admissions` = technical_probation_violation_admissions,
         `Parole Technical Admissions` = technical_parole_violation_admissions,
         `New Offense Admissions` = total_new_offense_admissions,
         `Probation New Offense Admissions` = new_offense_probation_violation_admissions,
         `Parole New Offense Admissions` = new_offense_parole_violation_admissions) %>%
  arrange(State)

adm_data_availability_table <- formattable(adm,
                                           table.attr = 'style="font-size: 14px; font-family: Graphik Regular";\"',
                                           align = c("l","l","c","c","c","c","c","c","c","c","c","c"),
                                           list(State = #formatter("span", style = x ~ style("font-weight" = "bold"), width = "200px"),
                                                  formatter(.tag = "span", style = function(x) style("font-weight" = "bold", "font-family" = "Graphik Regular",display = "inline-block", width = "120px")),
                                                `Total Admissions` = yes_no_fmt,
                                                `Supervision Violation Admissions` = yes_no_fmt,
                                                `Probation Violation Admissions` = yes_no_fmt,
                                                `Parole Violation Admissions` = yes_no_fmt,
                                                `Technical Admissions` = yes_no_fmt,
                                                `Probation Technical Admissions` = yes_no_fmt,
                                                `Parole Technical Admissions` = yes_no_fmt,
                                                `New Offense Admissions` = yes_no_fmt,
                                                `Probation New Offense Admissions` = yes_no_fmt,
                                                `Parole New Offense Admissions` = yes_no_fmt))

pop <- pop_all %>% mutate(
  across(.cols = total_prison_population:new_offense_parole_violation_population,
         .fns = ~ case_when(
           is.numeric(.x) & !is.na(.x) ~ 'Yes',
           is.numeric(.x) & is.na(.x)  ~ 'No'))) %>%
  #filter(year == 2021) %>%
  #filter_all(any_vars(grepl("No", .))) %>%
  select(`State` = state,
         `Year` = year,
         `Total Population` = total_prison_population,
         `Supervision Violation Population` = total_supervision_violation_population,
         `Probation Violation Population` = probation_violation_population,
         `Parole Violation Population` = parole_violation_population,
         `Technical Population` = total_technical_violation_population,
         `Probation Technical Population` = technical_probation_violation_population,
         `Parole Technical Population` = technical_parole_violation_population,
         `New Offense Population` = total_new_offense_population,
         `Probation New Offense Population` = new_offense_probation_violation_population,
         `Parole New Offense Population` = new_offense_parole_violation_population) %>%
  arrange(State)

pop_data_availability_table <- formattable(pop,
                                           table.attr = 'style="font-size: 14px; font-family: Graphik Regular";\"',
                                           align = c("l","l","c","c","c","c","c","c","c","c","c","c"),
                                           list(State = #formatter("span", style = x ~ style("font-weight" = "bold"), width = "200px"),
                                                  formatter(.tag = "span", style = function(x) style("font-weight" = "bold", "font-family" = "Graphik Regular",display = "inline-block", width = "120px")),
                                                `Total Population` = yes_no_fmt,
                                                `Supervision Violation Population` = yes_no_fmt,
                                                `Probation Violation Population` = yes_no_fmt,
                                                `Parole Violation Population` = yes_no_fmt,
                                                `Technical Population` = yes_no_fmt,
                                                `Probation Technical Population` = yes_no_fmt,
                                                `Parole Technical Population` = yes_no_fmt,
                                                `New Offense Population` = yes_no_fmt,
                                                `Probation New Offense Population` = yes_no_fmt,
                                                `Parole New Offense Population` = yes_no_fmt))

export_formattable <- function(f, file, width = 1150, height = NULL,
                               background = "white", delay = 10)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

write.csv(pop, "pop_data_availability_table_allyrs_v1.csv")
write.csv(adm, "adm_data_availability_table_allyrs_v1.csv")

export_formattable(pop_data_availability_table,"pop_data_availability_table_allyrs_v1.png")
export_formattable(adm_data_availability_table,"adm_data_availability_table_allyrs_v1.png")

#########################################################################################################
# Option with all years
#########################################################################################################

###################################
# Admissions
###################################

states_data <- adm %>%
  group_by(State) %>%
  summarise_all(~ toString(unique(.)))

# states that submitted the same data each year (could be Yeses or Nos)
states_same <- states_data %>%
  filter(across(3) != "Yes, No" &
           across(4) != "Yes, No" &
           across(5) != "Yes, No" &
           across(6) != "Yes, No" &
           across(7) != "Yes, No" &
           across(8) != "Yes, No" &
           across(9) != "Yes, No" &
           across(10) != "Yes, No"&
           across(11) != "Yes, No"&
           across(12) != "Yes, No")

# states that submitted differently for some years
states_diff <- states_data %>%
  filter(across(3) == "Yes, No" |
           across(4) == "Yes, No" |
           across(5) == "Yes, No" |
           across(6) == "Yes, No" |
           across(7) == "Yes, No" |
           across(8) == "Yes, No" |
           across(9) == "Yes, No" |
           across(10) == "Yes, No" |
           across(11) == "Yes, No" |
           across(12) == "Yes, No")

# manually assign years based on State - can't figure out how to do this programmatically at this time
states_diff <- adm %>%
  anti_join(states_same, by = "State")
# group_by(State, Year) %>%
# mutate(id = cur_group_id()) %>%

states_diff_manual <- states_diff %>%
  mutate(Year = case_when(
    State == "Alaska" & Year == "2018" ~ "2018 - 2020",
    State == "Alaska" & Year == "2019" ~ "2018 - 2020",
    State == "Alaska" & Year == "2020" ~ "2018 - 2020",
    State == "Alaska" & Year == "2021" ~ "2021",

    State == "Maine" & Year == "2018" ~ "2018 - 2020",
    State == "Maine" & Year == "2019" ~ "2018 - 2020",
    State == "Maine" & Year == "2020" ~ "2018 - 2020",
    State == "Maine" & Year == "2021" ~ "2021",

    State == "Maryland" & Year == "2018" ~ "2018 - 2020",
    State == "Maryland" & Year == "2019" ~ "2018 - 2020",
    State == "Maryland" & Year == "2020" ~ "2018 - 2020",
    State == "Maryland" & Year == "2021" ~ "2021",

    State == "Nebraska" & Year == "2018" ~ "2018 - 2019",
    State == "Nebraska" & Year == "2019" ~ "2018 - 2019",
    State == "Nebraska" & Year == "2020" ~ "2020 - 2021",
    State == "Nebraska" & Year == "2021" ~ "2020 - 2021",

    State == "New Hampshire" & Year == "2018" ~ "2018 - 2020",
    State == "New Hampshire" & Year == "2019" ~ "2018 - 2020",
    State == "New Hampshire" & Year == "2020" ~ "2018 - 2020",
    State == "New Hampshire" & Year == "2021" ~ "2021",

    State == "New Jersey" & Year == "2018" ~ "2018 - 2020",
    State == "New Jersey" & Year == "2019" ~ "2018 - 2020",
    State == "New Jersey" & Year == "2020" ~ "2018 - 2020",
    State == "New Jersey" & Year == "2021" ~ "2021",

    State == "New Mexico" & Year == "2018" ~ "2018 - 2019",
    State == "New Mexico" & Year == "2019" ~ "2018 - 2019",
    State == "New Mexico" & Year == "2020" ~ "2020 - 2021",
    State == "New Mexico" & Year == "2021" ~ "2020 - 2021"
  )) %>%
  distinct()

adm2 <- rbind(states_same, states_diff_manual)
adm2 <- adm2 %>%
  arrange(State) %>%
  mutate(Year = case_when(
    Year == "2018, 2019, 2020, 2021" ~ "2018 - 2021",
    TRUE ~ Year
  ))

adm_data_availability_table2 <- formattable(adm2,
                                           table.attr = 'style="font-size: 14px; font-family: Graphik Regular";\"',
                                           align = c("l","l","c","c","c","c","c","c","c","c","c","c"),
                                           list(State = #formatter("span", style = x ~ style("font-weight" = "bold"), width = "200px"),
                                                  formatter(.tag = "span", style = function(x) style("font-weight" = "bold", "font-family" = "Graphik Regular",display = "inline-block", width = "120px")),
                                                Year = year_fmt,
                                                `Total Admissions` = yes_no_fmt,
                                                `Supervision Violation Admissions` = yes_no_fmt,
                                                `Probation Violation Admissions` = yes_no_fmt,
                                                `Parole Violation Admissions` = yes_no_fmt,
                                                `Technical Admissions` = yes_no_fmt,
                                                `Probation Technical Admissions` = yes_no_fmt,
                                                `Parole Technical Admissions` = yes_no_fmt,
                                                `New Offense Admissions` = yes_no_fmt,
                                                `Probation New Offense Admissions` = yes_no_fmt,
                                                `Parole New Offense Admissions` = yes_no_fmt))

###################################
# Population
###################################

states_data <- pop %>%
  group_by(State) %>%
  summarise_all(~ toString(unique(.)))

# states that submitted the same data each year (could be Yeses or Nos)
states_same <- states_data %>%
  filter(across(3) != "Yes, No" &
           across(4) != "Yes, No" &
           across(5) != "Yes, No" &
           across(6) != "Yes, No" &
           across(7) != "Yes, No" &
           across(8) != "Yes, No" &
           across(9) != "Yes, No" &
           across(10) != "Yes, No"&
           across(11) != "Yes, No"&
           across(12) != "Yes, No")

# states that submitted differently for some years
states_diff <- states_data %>%
  filter(across(3) == "Yes, No" |
           across(4) == "Yes, No" |
           across(5) == "Yes, No" |
           across(6) == "Yes, No" |
           across(7) == "Yes, No" |
           across(8) == "Yes, No" |
           across(9) == "Yes, No" |
           across(10) == "Yes, No" |
           across(11) == "Yes, No" |
           across(12) == "Yes, No")

# manually assign years based on State - can't figure out how to do this programmatically at this time
states_diff <- pop %>%
  anti_join(states_same, by = "State")
# group_by(State, Year) %>%
# mutate(id = cur_group_id()) %>%

states_diff_manual <- states_diff %>%
  mutate(Year = case_when(
    State == "Alaska" & Year == "2018" ~ "2018 - 2020",
    State == "Alaska" & Year == "2019" ~ "2018 - 2020",
    State == "Alaska" & Year == "2020" ~ "2018 - 2020",
    State == "Alaska" & Year == "2021" ~ "2021",

    State == "Maryland" & Year == "2018" ~ "2018 - 2020",
    State == "Maryland" & Year == "2019" ~ "2018 - 2020",
    State == "Maryland" & Year == "2020" ~ "2018 - 2020",
    State == "Maryland" & Year == "2021" ~ "2021",

    State == "Nebraska" & Year == "2018" ~ "2018 - 2019",
    State == "Nebraska" & Year == "2019" ~ "2018 - 2019",
    State == "Nebraska" & Year == "2020" ~ "2020 - 2021",
    State == "Nebraska" & Year == "2021" ~ "2020 - 2021",

    State == "New Mexico" & Year == "2018" ~ "2018 - 2019",
    State == "New Mexico" & Year == "2019" ~ "2018 - 2019",
    State == "New Mexico" & Year == "2020" ~ "2020 - 2021",
    State == "New Mexico" & Year == "2021" ~ "2020 - 2021"
  )) %>%
  distinct()

pop2 <- rbind(states_same, states_diff_manual)
pop2 <- pop2 %>%
  arrange(State) %>%
  mutate(Year = case_when(
    Year == "2018, 2019, 2020, 2021" ~ "2018 - 2021",
    TRUE ~ Year
  ))

pop_data_availability_table2 <- formattable(pop2,
                                           table.attr = 'style="font-size: 14px; font-family: Graphik Regular";\"',
                                           align = c("l","l","c","c","c","c","c","c","c","c","c","c"),
                                           list(State = #formatter("span", style = x ~ style("font-weight" = "bold"), width = "200px"),
                                                  formatter(.tag = "span", style = function(x) style("font-weight" = "bold", "font-family" = "Graphik Regular",display = "inline-block", width = "120px")),
                                                Year = year_fmt,
                                                `Total Population` = yes_no_fmt,
                                                `Supervision Violation Population` = yes_no_fmt,
                                                `Probation Violation Population` = yes_no_fmt,
                                                `Parole Violation Population` = yes_no_fmt,
                                                `Technical Population` = yes_no_fmt,
                                                `Probation Technical Population` = yes_no_fmt,
                                                `Parole Technical Population` = yes_no_fmt,
                                                `New Offense Population` = yes_no_fmt,
                                                `Probation New Offense Population` = yes_no_fmt,
                                                `Parole New Offense Population` = yes_no_fmt))

export_formattable <- function(f, file, width = 1200, height = NULL,
                               background = "white", delay = 10)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

write.csv(pop2, "pop_data_availability_table_allyrs_v2.csv")
write.csv(adm2, "adm_data_availability_table_allyrs_v2.csv")

export_formattable(pop_data_availability_table2,"pop_data_availability_table_allyrs_v2.png")
export_formattable(adm_data_availability_table2,"adm_data_availability_table_allyrs_v2.png")
