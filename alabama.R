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
  rename(check_2018_21_22 = x2018,
         check_2019_21_22 = x2019,
         check_2020_21_22 = x2020) %>%
  filter(variable != "state") %>%
  rename(metric = variable) %>%
  mutate(metric = gsub("_21_22", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("check_", "", metric, fixed=TRUE)) %>%
  mutate(metric = gsub("_", " ", metric, fixed=TRUE)) %>%
  mutate(metric = str_to_title(metric))

alabama_adm <- merge(alabama_adm_21, alabama_adm_22, by = "metric", all.x = TRUE, all.y = TRUE)
alabama_adm <- merge(alabama_adm, alabama_adm_21_22, by = "metric", all.x = TRUE, all.y = TRUE)
alabama_adm <- alabama_adm %>%
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
  current_2018 = as.numeric(current_2018),
  current_2019 = as.numeric(current_2019),
  current_2020 = as.numeric(current_2020),
  current_2021 = as.numeric(current_2021),
  state = state_name) %>%
  arrange(order) %>%
  select(-order)
