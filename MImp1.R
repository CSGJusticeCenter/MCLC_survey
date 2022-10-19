#New National Variables/columns - REQUIRES UPDATING!
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = Total.Prison.Admissions,
  admissions_for_violations           = Total.Supervision.Violation.Admissions,
  admissions_for_technical_violations = Technical.Probation.Violation.Admissions + Technical.Parole.Violation.Admissions,
  admissions_for_new_crime_violations = New.Offense.Probation.Violation.Admissions + New.Offense.Parole.Violation.Admissions,
  overall_population                  = Total.Prison.Population,
  violator_population                 = Total.Supervision.Violation.Population,
  technical_violator_population       = Technical.Probation.Violation.Population + Technical.Parole.Violation.Population,
  new_crime_violator_population       = New.Offense.Probation.Violation.Population + New.Offense.Parole.Violation.Population
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
