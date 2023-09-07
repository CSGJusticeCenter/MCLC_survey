#######################################
# MCLC Survey
# Checks data submissions to see if data is repeated across metrics
# by MR/JSM
# Last updated: April 19, 2023 (MAR)

# Checks version 9 of data (4/18/2023)
#######################################

# check to see if states input the same values twice
check_repeated_values <- adm_pop_analysis_with_bjs_orig %>%

  # change data to numeric
  mutate(states = as.factor(states)) %>%
  mutate_if(is.character, as.numeric) %>%
  # replace all zeros with NA - no states should have zeros
  mutate_at(vars(c(-"states")), ~ case_when(.==0 ~ NA, TRUE ~ .)) %>%

  mutate(

    ##############
    # Admissions
    ##############

    `Supervision Violation Admissions = Technical Violation Admissions`
    = case_when(is.na(total_supervision_violation_admissions)                                  ~ NA,
                is.na(total_technical_violation_admissions)                                    ~ NA,
                total_supervision_violation_admissions == total_technical_violation_admissions ~ "Yes",
                total_supervision_violation_admissions != total_technical_violation_admissions ~ "No",
                TRUE ~ NA),

    `Supervision Violation Admissions = New Offense Violation Admissions`
    = case_when(is.na(total_supervision_violation_admissions)                                    ~ NA,
                is.na(total_new_offense_violation_admissions)                                    ~ NA,
                total_supervision_violation_admissions == total_new_offense_violation_admissions ~ "Yes",
                total_supervision_violation_admissions != total_new_offense_violation_admissions ~ "No",
                TRUE ~ NA),

    `Supervision Violation Admissions = Parole Violation Admissions`
    = case_when(is.na(total_supervision_violation_admissions)                         ~ NA,
                is.na(parole_violation_admissions)                                    ~ NA,
                total_supervision_violation_admissions == parole_violation_admissions ~ "Yes",
                total_supervision_violation_admissions != parole_violation_admissions ~ "No",
                TRUE ~ NA),

    `Supervision Violation Admissions = Probation Violation Admissions`
    = case_when(is.na(total_supervision_violation_admissions)                            ~ NA,
                is.na(probation_violation_admissions)                                    ~ NA,
                total_supervision_violation_admissions == probation_violation_admissions ~ "Yes",
                total_supervision_violation_admissions != probation_violation_admissions ~ "No",
                TRUE ~ NA),

    `Parole Violation Admissions = Parole New Offense Violation Admissions`
    = case_when(is.na(parole_violation_admissions)                                     ~ NA,
                is.na(new_offense_parole_violation_admissions)                         ~ NA,
                parole_violation_admissions == new_offense_parole_violation_admissions ~ "Yes",
                parole_violation_admissions != new_offense_parole_violation_admissions ~ "No",
                TRUE ~ NA),

    `Parole Violation Admissions = Parole Technical Violation Admissions`
    = case_when(is.na(parole_violation_admissions)                                   ~ NA,
                is.na(technical_parole_violation_admissions)                         ~ NA,
                parole_violation_admissions == technical_parole_violation_admissions ~ "Yes",
                parole_violation_admissions != technical_parole_violation_admissions ~ "No",
                TRUE ~ NA),

    `Probation Violation Admissions = Probation New Offense Violation Admissions`
    = case_when(is.na(probation_violation_admissions)                                        ~ NA,
                is.na(new_offense_probation_violation_admissions)                            ~ NA,
                probation_violation_admissions == new_offense_probation_violation_admissions ~ "Yes",
                probation_violation_admissions != new_offense_probation_violation_admissions ~ "No",
                TRUE ~ NA),

    `Probation Violation Admissions = Probation Technical Violation Admissions`
    = case_when(is.na(probation_violation_admissions)                                      ~ NA,
                is.na(technical_probation_violation_admissions)                            ~ NA,
                probation_violation_admissions == technical_probation_violation_admissions ~ "Yes",
                probation_violation_admissions != technical_probation_violation_admissions ~ "No",
                TRUE ~ NA),

    `New Offense Violation Admissions = Probation New Offense Admissions`
    = case_when(is.na(total_new_offense_violation_admissions)                                        ~ NA,
                is.na(new_offense_probation_violation_admissions)                                    ~ NA,
                total_new_offense_violation_admissions == new_offense_probation_violation_admissions ~ "Yes",
                total_new_offense_violation_admissions != new_offense_probation_violation_admissions ~ "No",
                TRUE ~ NA),

    `New Offense Violation Admissions = Parole New Offense Admissions`
    = case_when(is.na(total_new_offense_violation_admissions)                                     ~ NA,
                is.na(new_offense_parole_violation_admissions)                                    ~ NA,
                total_new_offense_violation_admissions == new_offense_parole_violation_admissions ~ "Yes",
                total_new_offense_violation_admissions != new_offense_parole_violation_admissions ~ "No",
                TRUE ~ NA),

    `Technical Violation Admissions = Probation Technical Admissions`
    = case_when(is.na(total_technical_violation_admissions)                                        ~ NA,
                is.na(technical_probation_violation_admissions)                                    ~ NA,
                total_technical_violation_admissions == technical_probation_violation_admissions ~ "Yes",
                total_technical_violation_admissions != technical_probation_violation_admissions ~ "No",
                TRUE ~ NA),

    `Technical Violation Admissions = Parole Technical Admissions`
    = case_when(is.na(total_technical_violation_admissions)                                     ~ NA,
                is.na(technical_parole_violation_admissions)                                    ~ NA,
                total_technical_violation_admissions == technical_parole_violation_admissions ~ "Yes",
                total_technical_violation_admissions != technical_parole_violation_admissions ~ "No",
                TRUE ~ NA),

    ##############
    # POPULATION
    ##############

    `Supervision Violation Population = Technical Violation Population`
    = case_when(is.na(total_supervision_violation_population)                                  ~ NA,
                is.na(total_technical_violation_population)                                    ~ NA,
                total_supervision_violation_population == total_technical_violation_population ~ "Yes",
                total_supervision_violation_population != total_technical_violation_population ~ "No",
                TRUE ~ NA),

    `Supervision Violation Population = New Offense Violation Population`
    = case_when(is.na(total_supervision_violation_population)                                    ~ NA,
                is.na(total_new_offense_violation_population)                                    ~ NA,
                total_supervision_violation_population == total_new_offense_violation_population ~ "Yes",
                total_supervision_violation_population != total_new_offense_violation_population ~ "No",
                TRUE ~ NA),

    `Supervision Violation Population = Parole Violation Population`
    = case_when(is.na(total_supervision_violation_population)                         ~ NA,
                is.na(parole_violation_population)                                    ~ NA,
                total_supervision_violation_population == parole_violation_population ~ "Yes",
                total_supervision_violation_population != parole_violation_population ~ "No",
                TRUE ~ NA),

    `Supervision Violation Population = Probation Violation Population`
    = case_when(is.na(total_supervision_violation_population)                            ~ NA,
                is.na(probation_violation_population)                                    ~ NA,
                total_supervision_violation_population == probation_violation_population ~ "Yes",
                total_supervision_violation_population != probation_violation_population ~ "No",
                TRUE ~ NA),

    `Parole Violation Population = Parole New Offense Violation Population`
    = case_when(is.na(parole_violation_population)                                     ~ NA,
                is.na(new_offense_parole_violation_population)                         ~ NA,
                parole_violation_population == new_offense_parole_violation_population ~ "Yes",
                parole_violation_population != new_offense_parole_violation_population ~ "No",
                TRUE ~ NA),

    `Parole Violation Population = Parole Technical Violation Population`
    = case_when(is.na(parole_violation_population)                                   ~ NA,
                is.na(technical_parole_violation_population)                         ~ NA,
                parole_violation_population == technical_parole_violation_population ~ "Yes",
                parole_violation_population != technical_parole_violation_population ~ "No",
                TRUE ~ NA),

    `Probation Violation Population = Probation New Offense Violation Population`
    = case_when(is.na(probation_violation_population)                                        ~ NA,
                is.na(new_offense_probation_violation_population)                            ~ NA,
                probation_violation_population == new_offense_probation_violation_population ~ "Yes",
                probation_violation_population != new_offense_probation_violation_population ~ "No",
                TRUE ~ NA),

    `Probation Violation Population = Probation Technical Violation Population`
    = case_when(is.na(probation_violation_population)                                      ~ NA,
                is.na(technical_probation_violation_population)                            ~ NA,
                probation_violation_population == technical_probation_violation_population ~ "Yes",
                probation_violation_population != technical_probation_violation_population ~ "No",
                TRUE ~ NA),

    `New Offense Violation Population = Probation New Offense Population`
    = case_when(is.na(total_new_offense_violation_population)                                        ~ NA,
                is.na(new_offense_probation_violation_population)                                    ~ NA,
                total_new_offense_violation_population == new_offense_probation_violation_population ~ "Yes",
                total_new_offense_violation_population != new_offense_probation_violation_population ~ "No",
                TRUE ~ NA),

    `New Offense Violation Population = Parole New Offense Population`
    = case_when(is.na(total_new_offense_violation_population)                                     ~ NA,
                is.na(new_offense_parole_violation_population)                                    ~ NA,
                total_new_offense_violation_population == new_offense_parole_violation_population ~ "Yes",
                total_new_offense_violation_population != new_offense_parole_violation_population ~ "No",
                TRUE ~ NA),

    `Technical Violation Population = Probation Technical Population`
    = case_when(is.na(total_technical_violation_population)                                        ~ NA,
                is.na(technical_probation_violation_population)                                    ~ NA,
                total_technical_violation_population == technical_probation_violation_population   ~ "Yes",
                total_technical_violation_population != technical_probation_violation_population   ~ "No",
                TRUE ~ NA),

    `Technical Violation Population = Parole Technical Population`
    = case_when(is.na(total_technical_violation_population)                                     ~ NA,
                is.na(technical_parole_violation_population)                                    ~ NA,
                total_technical_violation_population == technical_parole_violation_population   ~ "Yes",
                total_technical_violation_population != technical_parole_violation_population   ~ "No",
                TRUE ~ NA)
    ) %>%

  select(states, year,
         `Supervision Violation Admissions = Technical Violation Admissions`:`Technical Violation Population = Parole Technical Population`) %>%
  distinct()

# check to see if data adds up in the way we expect
check_math <- adm_pop_analysis_with_bjs_orig %>%

  # change data to numeric
  mutate(states = as.factor(states)) %>%
  mutate_if(is.character, as.numeric) %>%
  # replace all zeros with NA - no states should have zeros
  mutate_at(vars(c(-"states")), ~ case_when(.==0 ~ NA, TRUE ~ .)) %>%

  mutate(

    ##############
    # Admissions
    ##############

    `Supervision Violation Admissions = Probation Violation Admissions + Parole Violation Admissions`
    = case_when(is.na(total_supervision_violation_admissions) ~ NA,
                is.na(probation_violation_admissions)         ~ NA,
                is.na(parole_violation_admissions)            ~ NA,
                total_supervision_violation_admissions == probation_violation_admissions + parole_violation_admissions ~ "Yes",
                TRUE ~ "No"),

    `Probation Violation Admissions = Probation New Offense Violation Admissions + Probation Technical Violation Admissions`
    = case_when(is.na(probation_violation_admissions)             ~ NA,
                is.na(new_offense_probation_violation_admissions) ~ NA,
                is.na(technical_probation_violation_admissions)   ~ NA,
                probation_violation_admissions == new_offense_probation_violation_admissions + technical_probation_violation_admissions ~ "Yes",
                TRUE ~ "No"),

    `Parole Violation Admissions = Parole New Offense Violation Admissions + Parole Technical Violation Admissions`
    = case_when(is.na(parole_violation_admissions)             ~ NA,
                is.na(new_offense_parole_violation_admissions) ~ NA,
                is.na(technical_parole_violation_admissions)   ~ NA,
                parole_violation_admissions == new_offense_parole_violation_admissions + technical_parole_violation_admissions ~ "Yes",
                TRUE ~ "No"),

    `New Offense Violation Admissions = Parole New Offense Violation Admissions + Probation New Offense Violation Admissions`
    = case_when(is.na(total_new_offense_violation_admissions)     ~ NA,
                is.na(new_offense_parole_violation_admissions)    ~ NA,
                is.na(new_offense_probation_violation_admissions) ~ NA,
                total_new_offense_violation_admissions == new_offense_parole_violation_admissions + new_offense_probation_violation_admissions ~ "Yes",
                TRUE ~ "No"),

    `Technical Violation Admissions = Parole Technical Violation Admissions + Probation Technical Violation Admissions`
    = case_when(is.na(total_technical_violation_admissions)     ~ NA,
                is.na(technical_parole_violation_admissions)    ~ NA,
                is.na(technical_probation_violation_admissions) ~ NA,
                total_technical_violation_admissions == technical_parole_violation_admissions + technical_probation_violation_admissions ~ "Yes",
                TRUE ~ "No"),

    ##############
    # Population
    ##############

    `Supervision Violation Population = Probation Violation Population + Parole Violation Population`
    = case_when(is.na(total_supervision_violation_population) ~ NA,
                is.na(probation_violation_population)         ~ NA,
                is.na(parole_violation_population)            ~ NA,
                total_supervision_violation_population == probation_violation_population + parole_violation_population ~ "Yes",
                TRUE ~ "No"),

    `Probation Violation Population = Probation New Offense Violation Population + Probation Technical Violation Population`
    = case_when(is.na(probation_violation_population)             ~ NA,
                is.na(new_offense_probation_violation_population) ~ NA,
                is.na(technical_probation_violation_population)   ~ NA,
                probation_violation_population == new_offense_probation_violation_population + technical_probation_violation_population ~ "Yes",
                TRUE ~ "No"),

    `Parole Violation Population = Parole New Offense Violation Population + Parole Technical Violation Population`
    = case_when(is.na(parole_violation_population)             ~ NA,
                is.na(new_offense_parole_violation_population) ~ NA,
                is.na(technical_parole_violation_population)   ~ NA,
                parole_violation_population == new_offense_parole_violation_population + technical_parole_violation_population ~ "Yes",
                TRUE ~ "No"),

    `New Offense Violation Population = Parole New Offense Violation Population + Probation New Offense Violation Population`
    = case_when(is.na(total_new_offense_violation_population)     ~ NA,
                is.na(new_offense_parole_violation_population)    ~ NA,
                is.na(new_offense_probation_violation_population) ~ NA,
                total_new_offense_violation_population == new_offense_parole_violation_population + new_offense_probation_violation_population ~ "Yes",
                TRUE ~ "No"),

    `Technical Violation Population = Parole Technical Violation Population + Probation Technical Violation Population`
    = case_when(is.na(total_technical_violation_population)     ~ NA,
                is.na(technical_parole_violation_population)    ~ NA,
                is.na(technical_probation_violation_population) ~ NA,
                total_technical_violation_population == technical_parole_violation_population + technical_probation_violation_population ~ "Yes",
                TRUE ~ "No"),


  ) %>%

  select(states, year,
         `Supervision Violation Admissions = Probation Violation Admissions + Parole Violation Admissions`:
           `Technical Violation Population = Parole Technical Violation Population + Probation Technical Violation Population`) %>%
  distinct()

# write.xlsx(check_math, file = paste0(sp_data_path, "/50 State Survey (2022)/Data/check_math.xlsx"))

