############################################
# Project:  MCLC Survey (2022)
# File: functions.r
# Last updated: August 21, 2022
# Author: Mari Roberts

# Custom functions to create state checklists
############################################

#############################################
# CONTACT INFO
#############################################

# custom function to extract name and email from google sheets used in checklists.R
fnc_contact_info <- function(df, state_name){
  name  <- df[18,2]
  email <- df[18,4]
  df1 <- as.data.frame(c(name, email))
  df1 <- df1 %>%
    mutate(state = state_name) %>%
    rename(name = 1,
           email = 2)
}

#############################################
# NOTES AND COMMENTS
#############################################

# custom function to extract notes and additional comments used in checklists.R
fnc_notes_comments <- function(df, state_name){
  notes    <- df[49,2]
  comments <- df[49,10]
  df1 <- as.data.frame(c(notes, comments))
  df1 <- df1 %>%
    mutate(state = state_name) %>%
    rename(notes = 1,
           comments = 2)
}

#############################################
# COSTS
#############################################

# Custom function to extract costs used in checklists.R
fnc_costs <- function(df, state_name){
  # clean variable names
  # extract cost data in spreadsheet
  df1 <- janitor::clean_names(df)
  df_costs <- df1 %>% select(year_2019 = x6,
                             year_2020 = x7,
                             year_2021 = x8)
  df_costs <- df_costs[46,]
  df_costs <- as.data.frame(df_costs)

  # indicate when data was left blank or NA was entered
  # if "null" then the respondent left the field blank
  # if "NA" (or variations of the spelling of NA) then the respondent input NA and we label it as "No Data"
  # add $ sign to dollar amounts
  df_costs <- df_costs %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . == "n/a" |
                                           . == "na" |
                                           . ==  "nodata" |
                                           . ==  "no data" |
                                           . ==  "notavailable" |
                                           . ==  "not available" |
                                           . ==  "notready" |
                                           . ==  "not ready" |
                                           . == "[none]" |
                                           . == "none"
                                         , "No Data"))) %>%
    # mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank", .))) %>%
    # mutate(across(everything(), ~replace(., . ==  "null", "Left Blank"))) %>%
    mutate(year_2019 = ifelse(is.na(year_2019) | year_2019 == "null", "No Data",    year_2019),
           year_2020 = ifelse(is.na(year_2020) | year_2020 == "null", "No Data",    year_2020)) %>%
    mutate(year_2021 = ifelse(is.na(year_2021) | year_2021 == "null", "Left Blank", year_2021)) %>%
    mutate(check_year_2019 = case_when(year_2019 == "No Data" ~ "No Data",
                                       year_2019 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ year_2019),
           check_year_2020 = case_when(year_2020 == "No Data" ~ "No Data",
                                       year_2020 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ year_2020),
           check_year_2021 = case_when(year_2021 == "No Data" ~ "No Data",
                                       year_2021 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ year_2021)) %>%
    mutate(across(c(year_2019, year_2020, year_2021), as.numeric)) %>%
    mutate_if(is.numeric,funs(formattable::comma(., digits = 2))) %>%
    mutate_if(is.numeric,funs(paste0("$", .))) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(state = state_name) %>%
    mutate(year_2019 = case_when(year_2019 == "$ NA" ~ check_year_2019,
                                 TRUE ~ year_2019),
           year_2020 = case_when(year_2020 == "$ NA" ~ check_year_2020,
                                 TRUE ~ year_2020),
           year_2021 = case_when(year_2021 == "$ NA" ~ check_year_2021,
                                 TRUE ~ year_2021)) %>%
    select(-c(check_year_2019, check_year_2020, check_year_2021))
}

#############################################
# DEFINITIONS CHECKLIST
#############################################

# custom function to extract definition confirmations used in checklists.R
# if they checked the box to confirm their definition then it is "Confirmed"
# if they did not check the box but left some notes, then it is also "Confirmed"
# if they did not check the box and did not leave notes, then it is "Not Confirmed"
fnc_definitions <- function(df, state_name){
  df1 <- janitor::clean_names(df)
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])
  df1 <- df1[c(22:31),]
  df1 <- df1 %>%
    select(metric, include = x19, dontinclude = x20, definition_confirmation = x14, definition_notes = x16) %>%
    mutate(include = gsub("CONFIRMED - ", "", include),
           dontinclude = gsub("CONFIRMED - ", "", dontinclude)) %>%
    mutate(state = state_name,
           metric = paste0(metric, "/Population", sep=""),
           definition_confirmation = case_when(definition_confirmation == "TRUE" ~ "Confirmed",
                                               definition_confirmation == "FALSE" & is.na(definition_notes) ~ "Not Confirmed",
                                               definition_confirmation == "FALSE" & !is.na(definition_notes)~ "Confirmed"
           ))
}

#############################################
# QA sentences
#############################################

# custom function that generates a data quality sentence depending on data issue
# for example, supervision violation admissions should equal the number of probation and parole violation admissions, otherwise leave blank in email
# if the data doesn't add up (check == TRUE), then generate a sentence saying that the data may be inaccurate
# admissions
fnc_qa_sentence_adm <- function(check, variable_1, variable_2, variable_3){
  variable_1<-eval(parse(text = "variable_1"))
  variable_2<-eval(parse(text = "variable_2"))
  variable_3<-eval(parse(text = "variable_3"))
  qa_sentence <- case_when(
    check == TRUE ~ paste("<br>Your data may be inaccurate. In most cases, the total number of ", variable_1, " should equal the number of ", variable_2,
                           " and ", variable_3, ".<br><br>",sep = ""),
    TRUE ~ "<span>")
}

# custom function that generates a data quality sentence depending on data issue
# for example, supervision violation population should equal the number of probation and parole violation populations, otherwise leave blank in email
# if the data doesn't add up (check == TRUE), then generate a sentence saying that the data may be inaccurate
# population
fnc_qa_sentence_pop <- function(check, variable_1, variable_2, variable_3){
  variable_1<-eval(parse(text = "variable_1"))
  variable_2<-eval(parse(text = "variable_2"))
  variable_3<-eval(parse(text = "variable_3"))
  qa_sentence <- case_when(
    check == TRUE ~ paste("<br>Your data may be inaccurate. In most cases, the ", variable_1, " should equal the sum of the ", variable_2,
                          " and ", variable_3, ".<br><br>",sep = ""),
    TRUE ~ "<span>")
  return(qa_sentence)
}

#############################################
# STATE DATA CHECKLIST
#############################################

# custom function to generate state data checklist used in checklists.R
# indicates if numbers don't add up, what was left blank, or no data
# there's a lot of code because one column can have a number and character data type
# and we want to add commas to the numbers while also retaining whether an NA is an actual NA or if it was left blank
fnc_create_state_data_checklist <- function(df, state_name){

  # clean variable names
  df1 <- janitor::clean_names(df)

  # combine columns of text
  df1$metric <- apply(df1[,1:3], 1, function(x) x[!is.na(x)][1])

  # select admissions and population data in spreadsheet
  # rename variables
  # remove white space and instructions that imported from Google Sheets
  df1 <- df1 %>% select(metric,
                        year_2018 = x5,
                        year_2019 = x6,
                        year_2020 = x7,
                        year_2021 = x8)
  df1 <- df1[c(22:31, 34:43),]

  # indicate when data was left blank, NA was entered
  # if "null" then the respondent left the field blank
  # if "NA" (or variations of the spelling of NA) then the respondent inputed this and we label it as "No Data"
  # df1 <- df1 %>%
  #   mutate(across(everything(), as.character)) %>%
  #   mutate_if(is.character, str_to_lower) %>%
  #   mutate_if(is.character, funs(ifelse(is.na(.), "Left Blank", .))) %>%
  #   mutate_if(grepl('null',.), ~replace(., grepl('null', .), "Left Blank")) %>%
  #   mutate(across(everything(), ~replace(., . ==  "na" |
  #                                          . ==  "nodata" |
  #                                          . ==  "no data" |
  #                                          . ==  "notavailable" |
  #                                          . ==  "not available" |
  #                                          . ==  "notready" |
  #                                          . ==  "not ready" |
  #                                          . == "[none]" |
  #                                          . == "none"
  #                                        , "No Data")))
  df1 <- df1 %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, str_to_lower) %>%
    mutate(across(everything(), ~replace(., . ==  "n/a" |
                                           . == "na" |
                                           . ==  "nodata" |
                                           . ==  "no data" |
                                           . ==  "notavailable" |
                                           . ==  "not available" |
                                           . ==  "notready" |
                                           . ==  "not ready" |
                                           . == "[none]" |
                                           . == "none"
                                         , "No Data"))) %>%
    mutate(year_2018 = ifelse(is.na(year_2018) | year_2018 == "null", "No Data",    year_2018),
           year_2019 = ifelse(is.na(year_2019) | year_2019 == "null", "No Data",    year_2019),
           year_2020 = ifelse(is.na(year_2020) | year_2020 == "null", "No Data",    year_2020)) %>%
    mutate(year_2021 = ifelse(is.na(year_2021) | year_2021 == "null", "Left Blank", year_2021))

  # need to add commas to numbers but the column is character because of Left Blank and No Data
  # which is info we want
  # create temporary columns that capture this info
  df1 <- df1 %>%
    mutate(check_year_2018 = case_when(year_2018 == "No Data" | year_2018 == "NA" ~ "No Data",
                                       year_2018 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2019 = case_when(year_2019 == "No Data" | year_2019 == "NA" ~ "No Data",
                                       year_2019 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2020 = case_when(year_2020 == "No Data" | year_2020 == "NA" ~ "No Data",
                                       year_2020 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"),
           check_year_2021 = case_when(year_2021 == "No Data" | year_2021 == "NA" ~ "No Data",
                                       year_2021 == "Left Blank" ~ "Left Blank",
                                       TRUE ~ "Complete"))

  # identify which NAs were "No Data" or "Left Blank"
  df_checks <- df1 %>%
    mutate(across(everything(), as.character)) %>%
    mutate(year_2018 = case_when(check_year_2018 == "No Data" ~ "No Data",
                                 check_year_2018 == "Left Blank" ~ "Left Blank",
                                 check_year_2018 == "Complete" ~ year_2018),
           year_2019 = case_when(check_year_2019 == "No Data" ~ "No Data",
                                 check_year_2019 == "Left Blank" ~ "Left Blank",
                                 check_year_2019 == "Complete" ~ year_2019),
           year_2020 = case_when(check_year_2020 == "No Data" ~ "No Data",
                                 check_year_2020 == "Left Blank" ~ "Left Blank",
                                 check_year_2020 == "Complete" ~ year_2020),
           year_2021 = case_when(check_year_2021 == "No Data" ~ "No Data",
                                 check_year_2021 == "Left Blank" ~ "Left Blank",
                                 check_year_2021 == "Complete" ~ year_2021)) %>%
    select(c(metric, check_year_2018, check_year_2019, check_year_2020, check_year_2021))

  # select variables
  df1 <- df1 %>% select(-c(check_year_2018, check_year_2019, check_year_2020, check_year_2021))

  # transpose data
  df_transposed <- as.data.frame(t(df1))

  # make first row header and get year
  # rename variables to indicatethe 2022 survey data
  df_transposed <- df_transposed %>%
    row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("year") %>%
    janitor::clean_names() %>%
    mutate(year = case_when(grepl("2018", year) ~ 2018,
                            grepl("2019", year) ~ 2019,
                            grepl("2020", year) ~ 2020,
                            grepl("2021", year) ~ 2021)) %>%
    rename_with(~ paste0(., "_22"), -c(year))
  # mutate(across(everything(), as.numeric))

  # check to see if numbers add up correctly
  # supervision violations = probation + parole violations
  # probation violations = technical probation + new offense probation
  # parole violations = technical parole + new offense parole
  # new offense violations = new offense probation + new offense parole
  # technical violations = technical probation + technical parole
  df_final <- df_transposed %>%
    mutate(check_other_prison_admissions_22 = as.numeric(total_prison_admissions_22) - as.numeric(total_supervision_violation_admissions_22),

           check_supervision_violation_admissions_22 = case_when(as.numeric(total_supervision_violation_admissions_22) == as.numeric(probation_violation_admissions_22) + as.numeric(parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(total_supervision_violation_admissions_22) != as.numeric(probation_violation_admissions_22) + as.numeric(parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 total_supervision_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 total_supervision_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_probation_violation_admissions_22   = case_when(as.numeric(probation_violation_admissions_22) == as.numeric(technical_probation_violation_admissions_22) + as.numeric(new_offense_probation_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(probation_violation_admissions_22) != as.numeric(technical_probation_violation_admissions_22) + as.numeric(new_offense_probation_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 probation_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 probation_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_parole_violation_admissions_22      = case_when(as.numeric(parole_violation_admissions_22) == as.numeric(technical_parole_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(parole_violation_admissions_22) != as.numeric(technical_parole_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 parole_violation_admissions_22 == "No Data" ~ "No Data",
                                                                 parole_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_new_offense_violation_admissions_22 = case_when(as.numeric(total_new_offense_admissions_22) == as.numeric(new_offense_probation_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Correct",
                                                                 as.numeric(total_new_offense_admissions_22) != as.numeric(new_offense_probation_violation_admissions_22) + as.numeric(new_offense_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                 total_new_offense_admissions_22 == "No Data" ~ "No Data",
                                                                 total_new_offense_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_total_technical_violation_admissions_22 = case_when(as.numeric(total_technical_violation_admissions_22) == as.numeric(technical_probation_violation_admissions_22) + as.numeric(technical_parole_violation_admissions_22) ~ "Correct",
                                                                     as.numeric(total_technical_violation_admissions_22) != as.numeric(technical_probation_violation_admissions_22) + as.numeric(technical_parole_violation_admissions_22) ~ "Doesn't Add Up",
                                                                     total_technical_violation_admissions_22 == "No Data" ~ "No Data",
                                                                     total_technical_violation_admissions_22 == "Left Blank" ~ "Left Blank",
                                                                     TRUE ~ "No Data"),

           check_other_prison_population_22 = as.numeric(total_prison_population_22) - as.numeric(total_supervision_violation_population_22),

           check_supervision_violation_population_22 = case_when(as.numeric(total_supervision_violation_population_22) == as.numeric(probation_violation_population_22) + as.numeric(parole_violation_population_22) ~ "Correct",
                                                                 as.numeric(total_supervision_violation_population_22) != as.numeric(probation_violation_population_22) + as.numeric(parole_violation_population_22) ~ "Doesn't Add Up",
                                                                 total_supervision_violation_population_22 == "No Data" ~ "No Data",
                                                                 total_supervision_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_probation_violation_population_22   = case_when(as.numeric(probation_violation_population_22) == as.numeric(technical_probation_violation_population_22) + as.numeric(new_offense_probation_violation_population_22) ~ "Correct",
                                                                 as.numeric(probation_violation_population_22) != as.numeric(technical_probation_violation_population_22) + as.numeric(new_offense_probation_violation_population_22) ~ "Doesn't Add Up",
                                                                 probation_violation_population_22 == "No Data" ~ "No Data",
                                                                 probation_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_parole_violation_population_22      = case_when(as.numeric(parole_violation_population_22) == as.numeric(technical_parole_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
                                                                 as.numeric(parole_violation_population_22) != as.numeric(technical_parole_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                 parole_violation_population_22 == "No Data" ~ "No Data",
                                                                 parole_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_new_offense_violation_population_22 = case_when(as.numeric(total_new_offense_population_22) == as.numeric(new_offense_probation_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Correct",
                                                                 as.numeric(total_new_offense_population_22) != as.numeric(new_offense_probation_violation_population_22) + as.numeric(new_offense_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                 total_new_offense_population_22 == "No Data" ~ "No Data",
                                                                 total_new_offense_population_22 == "Left Blank" ~ "Left Blank",
                                                                 TRUE ~ "No Data"),

           check_total_technical_violation_population_22 = case_when(as.numeric(total_technical_violation_population_22) == as.numeric(technical_probation_violation_population_22) + as.numeric(technical_parole_violation_population_22) ~ "Correct",
                                                                     as.numeric(total_technical_violation_population_22) != as.numeric(technical_probation_violation_population_22) + as.numeric(technical_parole_violation_population_22) ~ "Doesn't Add Up",
                                                                     total_technical_violation_population_22 == "No Data" ~ "No Data",
                                                                     total_technical_violation_population_22 == "Left Blank" ~ "Left Blank",
                                                                     TRUE ~ "No Data"),
           state = state_name)

  # transpose data
  df_transposed_checks <- as.data.frame(t(df_checks))

  # make first row header and get year
  df_transposed_checks <- df_transposed_checks %>%
    row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    tibble::rownames_to_column("year") %>%
    mutate(year = case_when(grepl("2018", year) ~ 2018,
                            grepl("2019", year) ~ 2019,
                            grepl("2020", year) ~ 2020,
                            grepl("2021", year) ~ 2021)) %>%
    rename_with(~ paste0("entry_check_", . , "_22"), -c(year))

  # merge with final data
  df_final <- merge(df_final, df_transposed_checks, by = "year")

  # add commas to numbers
  df_final <- df_final %>%
    mutate(total_prison_admissions_22                    = formattable::comma(total_prison_admissions_22, digits = 0),
           total_supervision_violation_admissions_22     = formattable::comma(total_supervision_violation_admissions_22, digits = 0),
           probation_violation_admissions_22             = formattable::comma(probation_violation_admissions_22, digits = 0),
           parole_violation_admissions_22                = formattable::comma(parole_violation_admissions_22, digits = 0),
           total_technical_violation_admissions_22       = formattable::comma(total_technical_violation_admissions_22, digits = 0),
           technical_probation_violation_admissions_22   = formattable::comma(technical_probation_violation_admissions_22, digits = 0),
           technical_parole_violation_admissions_22      = formattable::comma(technical_parole_violation_admissions_22, digits = 0),
           total_new_offense_admissions_22               = formattable::comma(total_new_offense_admissions_22, digits = 0),
           new_offense_probation_violation_admissions_22 = formattable::comma(new_offense_probation_violation_admissions_22, digits = 0),
           new_offense_parole_violation_admissions_22    = formattable::comma(new_offense_parole_violation_admissions_22, digits = 0),

           total_prison_population_22                    = formattable::comma(total_prison_population_22, digits = 0),
           total_supervision_violation_population_22     = formattable::comma(total_supervision_violation_population_22, digits = 0),
           probation_violation_population_22             = formattable::comma(probation_violation_population_22, digits = 0),
           parole_violation_population_22                = formattable::comma(parole_violation_population_22, digits = 0),
           total_technical_violation_population_22       = formattable::comma(total_technical_violation_population_22, digits = 0),
           technical_probation_violation_population_22   = formattable::comma(technical_probation_violation_population_22, digits = 0),
           technical_parole_violation_population_22      = formattable::comma(technical_parole_violation_population_22, digits = 0),
           total_new_offense_population_22               = formattable::comma(total_new_offense_population_22, digits = 0),
           new_offense_probation_violation_population_22 = formattable::comma(new_offense_probation_violation_population_22, digits = 0),
           new_offense_parole_violation_population_22    = formattable::comma(new_offense_parole_violation_population_22, digits = 0),

           check_other_prison_admissions_22              = formattable::comma(check_other_prison_admissions_22, digits = 0),
           check_other_prison_population_22              = formattable::comma(check_other_prison_population_22, digits = 0))

  # identify NA vs Left Blank
  df_final <- df_final %>%
    mutate(across(everything(), as.character))  %>%

    mutate(total_prison_admissions_22                     = case_when(total_prison_admissions_22 == "NA"                     ~ entry_check_total_prison_admissions_22,
                                                                      TRUE ~ total_prison_admissions_22),
           total_supervision_violation_admissions_22      = case_when(total_supervision_violation_admissions_22 == "NA"      ~ entry_check_total_supervision_violation_admissions_22,
                                                                      TRUE ~ total_supervision_violation_admissions_22),
           probation_violation_admissions_22              = case_when(probation_violation_admissions_22 == "NA"              ~ entry_check_probation_violation_admissions_22,
                                                                      TRUE ~ probation_violation_admissions_22),
           parole_violation_admissions_22                 = case_when(parole_violation_admissions_22 == "NA"                 ~ entry_check_parole_violation_admissions_22,
                                                                      TRUE ~ parole_violation_admissions_22),
           total_technical_violation_admissions_22        = case_when(total_technical_violation_admissions_22 == "NA"        ~ entry_check_total_technical_violation_admissions_22,
                                                                      TRUE ~ total_technical_violation_admissions_22),
           technical_probation_violation_admissions_22    = case_when(technical_probation_violation_admissions_22 == "NA"    ~ entry_check_technical_probation_violation_admissions_22,
                                                                      TRUE ~ technical_probation_violation_admissions_22),
           technical_parole_violation_admissions_22       = case_when(technical_parole_violation_admissions_22 == "NA"       ~ entry_check_technical_parole_violation_admissions_22,
                                                                      TRUE ~ technical_parole_violation_admissions_22),
           total_new_offense_admissions_22                = case_when(total_new_offense_admissions_22 == "NA"                ~ entry_check_total_new_offense_admissions_22,
                                                                      TRUE ~ total_new_offense_admissions_22),
           new_offense_probation_violation_admissions_22  = case_when(new_offense_probation_violation_admissions_22 == "NA"  ~ entry_check_new_offense_probation_violation_admissions_22,
                                                                      TRUE ~ new_offense_probation_violation_admissions_22),
           new_offense_parole_violation_admissions_22     = case_when(new_offense_parole_violation_admissions_22 == "NA"     ~ entry_check_new_offense_parole_violation_admissions_22,
                                                                      TRUE ~ new_offense_parole_violation_admissions_22),
           total_prison_population_22                     = case_when(total_prison_population_22 == "NA"                     ~ entry_check_total_prison_population_22,
                                                                      TRUE ~ total_prison_population_22),
           total_supervision_violation_population_22      = case_when(total_supervision_violation_population_22 == "NA"      ~ entry_check_total_supervision_violation_population_22,
                                                                      TRUE ~ total_supervision_violation_population_22),
           probation_violation_population_22              = case_when(probation_violation_population_22 == "NA"              ~ entry_check_probation_violation_population_22,
                                                                      TRUE ~ probation_violation_population_22),
           parole_violation_population_22                 = case_when(parole_violation_population_22 == "NA"                 ~ entry_check_parole_violation_population_22,
                                                                      TRUE ~ parole_violation_population_22),
           total_technical_violation_population_22        = case_when(total_technical_violation_population_22 == "NA"        ~ entry_check_total_technical_violation_population_22,
                                                                      TRUE ~ total_technical_violation_population_22),
           technical_probation_violation_population_22    = case_when(technical_probation_violation_population_22 == "NA"    ~ entry_check_technical_probation_violation_population_22,
                                                                      TRUE ~ technical_probation_violation_population_22),
           technical_parole_violation_population_22       = case_when(technical_parole_violation_population_22 == "NA"       ~ entry_check_technical_parole_violation_population_22,
                                                                      TRUE ~ technical_parole_violation_population_22),
           total_new_offense_population_22                = case_when(total_new_offense_population_22 == "NA"                ~ entry_check_total_new_offense_population_22,
                                                                      TRUE ~ total_new_offense_population_22),
           new_offense_probation_violation_population_22  = case_when(new_offense_probation_violation_population_22 == "NA"  ~ entry_check_new_offense_probation_violation_population_22,
                                                                      TRUE ~ new_offense_probation_violation_population_22),
           new_offense_parole_violation_population_22     = case_when(new_offense_parole_violation_population_22 == "NA"     ~ entry_check_new_offense_parole_violation_population_22,
                                                                      TRUE ~ new_offense_parole_violation_population_22)) %>%
    mutate(across(everything(), as.character)) %>%
    mutate_if(is.character, funs(ifelse(is.na(.), "No Data", .)))

  return(df_final)
}

#############################################
# ADMISSIONS TABLE
#############################################

# admissions table
fnc_adm_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_adm_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_21:new_offense_parole_violation_admissions_21)

  # filter data to state and select 2022 data
  df_adm_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_admissions_22:new_offense_parole_violation_admissions_22)

  # filter data to state and select data quality checks in previous survey checklist (where 2018-2022 numbers changed?)
  df_adm_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_admissions_21_22:check_new_offense_parole_violation_admissions_21_22)

  # reshape data
  df_adm_21 <- reshape2::dcast(reshape2::melt(df_adm_21, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_21 <- df_adm_21 %>%
    clean_names() %>%
    rename(previous_2018 = x2018,
           previous_2019 = x2019,
           previous_2020 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_adm_22 <- reshape2::dcast(reshape2::melt(df_adm_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_22 <- df_adm_22 %>%
    clean_names() %>%
    rename(current_2018 = x2018,
           current_2019 = x2019,
           current_2020 = x2020,
           current_2021 = x2021,
           metric = variable) %>%
    mutate(metric = gsub("_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_adm_check_21_22 <- reshape2::dcast(reshape2::melt(df_adm_check_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_adm_check_21_22 <- df_adm_check_21_22 %>%
    clean_names() %>%
    rename(check_2018_21_22 = x2018,
           check_2019_21_22 = x2019,
           check_2020_21_22 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # add data together and order metrics in table
  df_adm <- merge(df_adm_21, df_adm_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_adm <- merge(df_adm, df_adm_check_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_adm <- df_adm %>%
    mutate(order = case_when(
      metric == "Total Prison Admissions"                     ~ 1,
      metric == "Total Supervision Violation Admissions"      ~ 2,
      metric == "Probation Violation Admissions"              ~ 3,
      metric == "Parole Violation Admissions"                 ~ 4,
      metric == "Total Technical Violation Admissions"        ~ 5,
      metric == "Technical Probation Violation Admissions"    ~ 6,
      metric == "Technical Parole Violation Admissions"       ~ 7,
      metric == "Total New Offense Admissions"                ~ 8,
      metric == "New Offense Probation Violation Admissions"  ~ 9,
      metric == "New Offense Parole Violation Admissions"     ~ 10
    ),
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}


#############################################
# POPULATION TABLE
#############################################

# population table
fnc_pop_table_checklist <- function(df, state_name){

  # filter data to state and select 2021 data
  df_pop_21 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_21:new_offense_parole_violation_population_21)

  # filter data to state and select 2022 data
  df_pop_22 <- state_data_checklist %>% filter(state == state_name) %>%
    select(year, total_prison_population_22:new_offense_parole_violation_population_22)

  # filter data to state and select data quality checks in previous survey checklist (where 2018-2022 numbers changed?)
  df_pop_check_21_22 <- survey_checklist %>% filter(state == state_name) %>%
    select(year, check_total_prison_population_21_22:check_new_offense_parole_violation_population_21_22)

  # reshape data
  df_pop_21 <- reshape2::dcast(reshape2::melt(df_pop_21, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_21 <- df_pop_21 %>%
    clean_names() %>%
    rename(previous_2018 = x2018,
           previous_2019 = x2019,
           previous_2020 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_pop_22 <- reshape2::dcast(reshape2::melt(df_pop_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_22 <- df_pop_22 %>%
    clean_names() %>%
    rename(current_2018 = x2018,
           current_2019 = x2019,
           current_2020 = x2020,
           current_2021 = x2021,
           metric = variable) %>%
    mutate(metric = gsub("_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # reshape data
  df_pop_check_21_22 <- reshape2::dcast(reshape2::melt(df_pop_check_21_22, id.vars = "year"), variable ~ year)

  # rename variables and rename metrics for table format
  df_pop_check_21_22 <- df_pop_check_21_22 %>%
    clean_names() %>%
    rename(check_2018_21_22 = x2018,
           check_2019_21_22 = x2019,
           check_2020_21_22 = x2020) %>%
    filter(variable != "state") %>%
    rename(metric = variable) %>%
    mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
    mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
    mutate(metric = str_to_title(metric))

  # add data together and order metrics in table
  df_pop <- merge(df_pop_21, df_pop_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_pop <- merge(df_pop, df_pop_check_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
  df_pop <- df_pop %>%
    mutate(order = case_when(
      metric == "Total Prison Population"                     ~ 1,
      metric == "Total Supervision Violation Population"      ~ 2,
      metric == "Probation Violation Population"              ~ 3,
      metric == "Parole Violation Population"                 ~ 4,
      metric == "Total Technical Violation Population"        ~ 5,
      metric == "Technical Probation Violation Population"    ~ 6,
      metric == "Technical Parole Violation Population"       ~ 7,
      metric == "Total New Offense Population"                ~ 8,
      metric == "New Offense Probation Violation Population"  ~ 9,
      metric == "New Offense Parole Violation Population"     ~ 10
    ),
    state = state_name) %>%
    arrange(order) %>%
    select(-order)
}

# organize columns for last years format
fnc_org_adm_columns <- function(df){
  df <- df %>% select(
    state,
    `Total Prison Admissions`,
    `Total Supervision Violation Admissions`,
    `Probation Violation Admissions`,
    `Technical Probation Violation Admissions`,
    `New Offense Probation Violation Admissions`,
    `Parole Violation Admissions`,
    `Technical Parole Violation Admissions`,
    `New Offense Parole Violation Admissions`,
    `Total New Offense Admissions`,
    `Total Technical Violation Admissions`
  )
}

fnc_org_pop_columns <- function(df){
  df <- df %>% select(
    state,
    `Total Prison Population`,
    `Total Supervision Violation Population`,
    `Probation Violation Population`,
    `Technical Probation Violation Population`,
    `New Offense Probation Violation Population`,
    `Parole Violation Population`,
    `Technical Parole Violation Population`,
    `New Offense Parole Violation Population`,
    `Total New Offense Population`,
    `Total Technical Violation Population`
  )
}
