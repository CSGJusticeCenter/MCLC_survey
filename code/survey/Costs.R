################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

#just for state-level cost report REQUIRES UPDATING!!!
forreport <- national.est[,c("violator_population2019","violator_population2020","violator_population2021")]

cost.final <- costs %>%
  mutate(year_2019 = as.numeric(year_2019),
         year_2020 = as.numeric(year_2020),
         year_2021 = as.numeric(year_2021)) %>%
  mutate(
    year_2020 = case_when(is.na(year_2020) ~ year_2019,
                          is.na(year_2021) ~ year_2020,
                          TRUE ~ year_2021)
  ) %>%
  bind_cols(forreport) %>%
  mutate(
    averted_costs = (violator_population2020*year_2021*365) - (violator_population2021*year_2021*365)
  )
