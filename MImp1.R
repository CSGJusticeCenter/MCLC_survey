#New National Variables/columns - REQUIRES UPDATING!
m.imp <- adm_pop_analysis %>% mutate(
  overall_admissions                  = Total.admissions,
  admissions_for_violations           = Total.violation.admissions,
  admissions_for_technical_violations = Technical.probation.violation.admissions + Technical.parole.violation.admissions,
  admissions_for_new_crime_violations = New.offense.probation.violation.admissions + New.offense.parole.violation.admissions,
  overall_population                  = Total.population,
  violator_population                 = Total.violation.population,
  technical_violator_population       = Technical.probation.violation.population + Technical.parole.violation.population,
  new_crime_violator_population       = New.offense.probation.violation.population + New.offense.parole.violation.population
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