#######
fit <- paste0("fit.",tablevals)
#CHANGE REFERENCE
#this is the multiple imputation list
for (h in refnum) {
  #start with 2019 as reference year, then proceeds to 2020, then 2021 until most current year of data
  temp_data$data$year <- relevel(as.factor(temp_data$data$year), ref = h+1)
  
  #response columns, run model to produce estimates/CIs
  fit.overall_admissions                  <- with(temp_data,lm(overall_admissions~year + states + year:states))
  fit.admissions_for_violations           <- with(temp_data,lm(admissions_for_violations~year + states + year:states))
  fit.admissions_for_technical_violations <- with(temp_data,lm(admissions_for_technical_violations~year + states + year:states))
  fit.admissions_for_new_crime_violations <- with(temp_data,lm(admissions_for_new_crime_violations~year + states + year:states))
  fit.overall_population                  <- with(temp_data,lm(overall_population~year + states + year:states))
  fit.violator_population                 <- with(temp_data,lm(violator_population~year + states + year:states))
  fit.technical_violator_population       <- with(temp_data,lm(technical_violator_population~year + states + year:states))
  fit.new_crime_violator_population       <- with(temp_data,lm(new_crime_violator_population~year + states + year:states))
  
  for (i in numvar) {
    CI             <- summary(pool(get(fit[i])),conf.int=TRUE) %>%
      select(term,estimate)
    CIt            <- as.data.frame(t(as.matrix(CI)))
    rownames(CIt)  <- colnames(CI)
    colnames(CIt)  <- CI[,1]
    CIt1           <- CIt %>% select(-contains("year"))
    CIt1           <- CIt1[-c(1),]
    colnames(CIt1) <- admissions19$state #this is just to get state names from data
    assign(tablevals[i],CIt1,envir=.GlobalEnv)
  }
  
  #rbind and finalize formatting
  national.est.CI            <- do.call(rbind,mget(tablevals))
  national.est.CIn           <- as.data.frame(sapply(national.est.CI, as.numeric))
  rownames(national.est.CIn) <- rownames(national.est.CI)
  national.est.CIn           <- national.est.CIn[,1:50] 
  national.est.CIn[,51]      <- national.est.CIn[,1]
  for (q in 2:50){
    national.est.CIn[,50+q]  <- national.est.CIn[,1] + national.est.CIn[,q]
  }
  national.est.CIn           <- national.est.CIn[,51:100]
  colnames(national.est.CIn) <- admissions19$state
  
  national.est.CIt           <- transpose(national.est.CIn)
  colnames(national.est.CIt) <- paste0(rownames(national.est.CIn),refyear[h])
  rownames(national.est.CIt) <- colnames(national.est.CIn)
  
  assign(paste0("national.est.CIt",refyear[h]),national.est.CIt,envir=.GlobalEnv)
}

#cbind each data frame: 2019, 2020, 2021 REQUIRES UPDATING
national.est                                   <- cbind(national.est.CIt2019,national.est.CIt2020,national.est.CIt2021)
national.est$new_crime_violator_population2019 <- as.numeric(format(round(national.est$new_crime_violator_population2019,2), nsmall=0,scientific = F, digits = 3))
national.est$new_crime_violator_population2020 <- as.numeric(format(round(national.est$new_crime_violator_population2020,2), nsmall=0,scientific = F, digits = 3))
national.est$new_crime_violator_population2021 <- as.numeric(format(round(national.est$new_crime_violator_population2021,2), nsmall=0,scientific = F, digits = 3))
