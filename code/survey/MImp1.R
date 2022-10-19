#New National Variables/columns - REQUIRES UPDATING!
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = total_prison_admissions,
  admissions_for_violations           = total_supervision_violation_admissions,
  admissions_for_technical_violations = technical_probation_violation_admissions + technical_parole_violation_admissions,
  admissions_for_new_crime_violations = new_offense_probation_violation_admissions + new_offense_parole_violation_admissions,

  overall_population                  = total_prison_population,
  violator_population                 = total_supervision_violation_population,
  technical_violator_population       = technical_probation_violation_population + technical_parole_violation_population,
  new_crime_violator_population       = new_offense_probation_violation_population + new_offense_parole_violation_population
) %>%
  select(states, year,
         overall_admissions,
         admissions_for_violations,
         admissions_for_technical_violations,
         admissions_for_new_crime_violations,
         overall_population,
         violator_population,
         technical_violator_population,
         new_crime_violator_population)

# missing data for a certain feature or sample is more than 5%
pMiss <- function(x){
  sum(is.na(x))/length(x)*100
}
apply(m.imp,2,pMiss)

# m=5 refers to the number of imputed datasets
# meth='pmm' refers to the imputation method, predictive mean matching
temp_data <- mice(m.imp,
                  m     = 5,
                  maxit = 50,
                  meth  = 'pmm',
                  seed  = 500)
summary(temp_data)

# check imputed data
# each observation (first column left) within each imputed data set (first row at the top)
temp_data$imp$overall_admissions

#each individual imputed data frame (m=5)
#use this for modeling code at the state level
for (i in 1:5) {
  micedata <- complete(temp_data,i)
  assign(paste0("mice_imputed_data",i),
         micedata,
         envir = .GlobalEnv)
}
