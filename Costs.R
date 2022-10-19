################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

#just for state-level cost report REQUIRES UPDATING!!!
forreport <- national.est[,c("violator_population2019","violator_population2020","violator_population2021")]

cost.final <- costs %>%
  mutate(
    Cost.in.2020 = case_when(is.na(Cost.in.2020) ~ Cost.in.2019,
                             TRUE ~ Cost.in.2020),
    Cost.in.2021 = case_when(is.na(Cost.in.2021) ~ Cost.in.2020,
                             TRUE ~ Cost.in.2021)
  ) %>% 
  bind_cols(forreport) %>%
  mutate(
    averted_costs = (violator_population2020*Cost.in.2021*365) - (violator_population2021*Cost.in.2021*365) 
  )
