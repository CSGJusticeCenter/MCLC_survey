################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

#just for state-level cost report REQUIRES UPDATING!!!
#community supervision violators, technical supervision violators populations
forreport <- national.est[,c("violator_population2019",          "violator_population2020",          "violator_population2021",
                             "technical_violator_population2019","technical_violator_population2020","technical_violator_population2021")]

#use non-imputed data for calculating costs by state
forreport.state <- adm_pop_analysis[,c("states","year","total_supervision_violation_population","technical_parole_violation_population","technical_probation_violation_population")] %>%
  filter(year==2021) %>%
  mutate(technical_violator_population.NA          = case_when(as.numeric(technical_probation_violation_population) >= 0 & as.numeric(technical_parole_violation_population) >= 0 ~ 
                                                                 as.numeric(technical_probation_violation_population) + as.numeric(technical_parole_violation_population),
                                                               TRUE ~ NA_real_),
         total_supervision_violation_population.NA = case_when(as.numeric(total_supervision_violation_population) >= 0 ~ as.numeric(total_supervision_violation_population),
                                                               TRUE ~ NA_real_)
         ) %>%
  arrange(states) %>%
  column_to_rownames(var="states") %>%
  select(-c(year,technical_probation_violation_population,technical_parole_violation_population,total_supervision_violation_population))

cost.final <- costs %>%
  mutate(
    Cost.in.2020 = case_when(is.na(Cost.in.2020) ~ Cost.in.2019,
                             TRUE ~ Cost.in.2020),
    Cost.in.2021 = case_when(is.na(Cost.in.2021) ~ Cost.in.2020,
                             TRUE ~ Cost.in.2021)
  ) %>% 
  bind_cols(forreport,forreport.state) %>%
  mutate(
    averted_costs20_21 = (violator_population2020*Cost.in.2021*365) - 
                         (violator_population2021*Cost.in.2021*365),
    averted_costs19_21 = (violator_population2019*Cost.in.2021*365) - 
                         (violator_population2021*Cost.in.2021*365),    
    averted_costs19_20 = (violator_population2019*Cost.in.2020*365) - 
                         (violator_population2020*Cost.in.2020*365),
    cost.vp21          = (Cost.in.2021*365*violator_population2021),
    cost.tvp21         = (Cost.in.2021*365*technical_violator_population2021),
    cost.vp21.state    = (Cost.in.2021*365*total_supervision_violation_population.NA),
    cost.tvp21.state   = (Cost.in.2021*365*technical_violator_population.NA)
    ) %>%
  dplyr::rename(State=state) 
