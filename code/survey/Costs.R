################################################################################
#COSTS
#
#Take state-level estimates and calculate costs
################################################################################

#just for state-level cost report REQUIRES UPDATING!!!
#all aggregations except overall
forreport <- national.est[,grepl( paste0("population",last(refyear)), names(national.est))] %>% select(-c(starts_with("overall")))

#use non-imputed data for calculating costs by state
forreport.state <- adm_pop_analysis[,c("states","year","total_supervision_violation_population","technical_parole_violation_population","technical_probation_violation_population")] %>%
  filter(year==as.numeric(last(refyear))) %>%
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
    #national estimated costs
    cost.pr.vp       = (Cost.in.2021*365*probation_violation_population2021),
    cost.pa.vp       = (Cost.in.2021*365*parole_violation_population2021),
    cost.pr.tvp      = (Cost.in.2021*365*technical_probation_violation_population2021),
    cost.pa.tvp      = (Cost.in.2021*365*technical_parole_violation_population2021),
    cost.pr.novp     = (Cost.in.2021*365*new_offense_probation_violation_population2021),
    cost.pa.novp    = (Cost.in.2021*365*new_offense_parole_violation_population2021),
    
    #state costs
    cost.vp.state    = (Cost.in.2021*365*total_supervision_violation_population.NA),
    cost.tvp.state   = (Cost.in.2021*365*technical_violator_population.NA)
    ) %>%
  dplyr::rename(State=state) 

##Annual costs for incarceration nationally (most recent year)
total.pr.vp   <- sum(cost.final$cost.pr.vp)
total.pa.vp   <- sum(cost.final$cost.pa.vp)
total.pr.tvp  <- sum(cost.final$cost.pr.tvp)
total.pa.tvp  <- sum(cost.final$cost.pa.tvp)
total.pr.novp <- sum(cost.final$cost.pr.novp)
total.pa.novp <- sum(cost.final$cost.pa.novp)