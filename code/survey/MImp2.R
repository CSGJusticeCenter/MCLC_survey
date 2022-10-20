#######
###aggregated data: national level
###IMPUTED DATA m=1,2,3,4,5
micelist <- c("mice_imputed_data1","mice_imputed_data2","mice_imputed_data3","mice_imputed_data4","mice_imputed_data5")
for (h in 1:5) {
  for (i in tablevals) {
    natmice <- setNames(aggregate(get(micelist[h])[[i]], 
                                  by=list(Category=get(micelist[h])$year), FUN=sum)[2],i)
    assign(paste0(i,h),natmice,envir = .GlobalEnv)
  }
}
for (i in 1:5) {
  nat <- do.call(cbind,mget(paste0(tablevals,i))) %>%
    mutate(across(where(is.numeric),formattable::comma,1)) %>%
    cbind(year)
  assign(paste0(micelist[i],".nat"),nat,envir = .GlobalEnv)
}
for (i in 1:5) {
  test <- get(paste0(micelist[i],".nat"))
  label(test)<-as.list(var.labels[match(names(get(paste0(micelist[i],".nat"))), names(var.labels))])
  assign(paste0(micelist[i],".nat"),test,envir = .GlobalEnv)
}
#calculate 95% CI for all counts across all years
#assume Poisson Distribution using count data
for (h in 1:5) {
  for (i in tablevals) {
    for (j in yearnum) { 
      test<-data.frame(CI95=poisson.test(get(paste0(micelist[h],".nat"))[j,i], conf.level = 0.95)$conf.int[1:2])
      colnames(test)[colnames(test)=="CI95"] <- paste0(i,"1.",j)
      assign(paste0(i,"1.",j,".m",h),test,envir = .GlobalEnv)
    }
  }
}
for (i in 1:5) {
  CI <- do.call(cbind,mget(c(paste0(tablevals,"1.1.m",i),paste0(tablevals,"1.2.m",i),paste0(tablevals,"1.3.m",i)))) %>%
    mutate(across(where(is.numeric),formattable::comma,1))
  assign(paste0(micelist[i],".natCI"),CI,envir = .GlobalEnv)
}
#rename columns to indicate which imputed data frame
for (i in 1:5) {
  nameit <- get(paste0(micelist[i],".nat"))
  names(nameit)[names(nameit) == "overall_admissions"]                  <- "overall_admissions1"
  names(nameit)[names(nameit) == "admissions_for_violations"]           <- "admissions_for_violations1"
  names(nameit)[names(nameit) == "admissions_for_technical_violations"] <- "admissions_for_technical_violations1"
  names(nameit)[names(nameit) == "admissions_for_new_crime_violations"] <- "admissions_for_new_crime_violations1"
  names(nameit)[names(nameit) == "overall_population"]                  <- "overall_population1"
  names(nameit)[names(nameit) == "violator_population"]                 <- "violator_population1"
  names(nameit)[names(nameit) == "technical_violator_population"]       <- "technical_violator_population1"
  names(nameit)[names(nameit) == "new_crime_violator_population"]       <- "new_crime_violator_population1"
  assign(paste0(micelist[i],".nat"),nameit,envir = .GlobalEnv)
}


##########################
#CALCULATE FINAL ESTIMATES, m=1,2,3,4,5 (i.e., calculate average)
#sum of theta estimates divided by total number of imputations
for (j in yearnum){
  parest <- data.frame(
    overall_admissions                  = (mice_imputed_data1.nat[j,1] + mice_imputed_data2.nat[j,1] + mice_imputed_data3.nat[j,1] + mice_imputed_data4.nat[j,1] + mice_imputed_data5.nat[j,1])/5,
    admissions_for_violations           = (mice_imputed_data1.nat[j,2] + mice_imputed_data2.nat[j,2] + mice_imputed_data3.nat[j,2] + mice_imputed_data4.nat[j,2] + mice_imputed_data5.nat[j,2])/5,
    admissions_for_technical_violations = (mice_imputed_data1.nat[j,3] + mice_imputed_data2.nat[j,3] + mice_imputed_data3.nat[j,3] + mice_imputed_data4.nat[j,3] + mice_imputed_data5.nat[j,3])/5,
    admissions_for_new_crime_violations = (mice_imputed_data1.nat[j,4] + mice_imputed_data2.nat[j,4] + mice_imputed_data3.nat[j,4] + mice_imputed_data4.nat[j,4] + mice_imputed_data5.nat[j,4])/5,
    overall_population                  = (mice_imputed_data1.nat[j,5] + mice_imputed_data2.nat[j,5] + mice_imputed_data3.nat[j,5] + mice_imputed_data4.nat[j,5] + mice_imputed_data5.nat[j,5])/5,
    violator_population                 = (mice_imputed_data1.nat[j,6] + mice_imputed_data2.nat[j,6] + mice_imputed_data3.nat[j,6] + mice_imputed_data4.nat[j,6] + mice_imputed_data5.nat[j,6])/5,
    technical_violator_population       = (mice_imputed_data1.nat[j,7] + mice_imputed_data2.nat[j,7] + mice_imputed_data3.nat[j,7] + mice_imputed_data4.nat[j,7] + mice_imputed_data5.nat[j,7])/5,
    new_crime_violator_population       = (mice_imputed_data1.nat[j,8] + mice_imputed_data2.nat[j,8] + mice_imputed_data3.nat[j,8] + mice_imputed_data4.nat[j,8] + mice_imputed_data5.nat[j,8])/5
  )
  assign(paste0("final.parest.imp",year[j]),parest,envir = .GlobalEnv)
}

#rbind final estimates REQUIRES UPDATING!!!!!!!!!
final.parest.imp      <- rbind(final.parest.imp2018,final.parest.imp2019,final.parest.imp2020,final.parest.imp2021)
final.parest.imp$year <- year

##########################
#CALCULATE FINAL CONFIDENCE INTERVALS, m=1,2,3,4,5 (i.e., calculate TOTAL variance)
# equation: w + (1+1/m)*b, where w=within-imputation variance, b=between-imputation variance, m=number of imputations
# w = sum of variance estimates divided by total number of imputations
# b = sum of the squared difference of the combined estimate (theta hat) from each theta estimate divided by the total number of imputations minus 1

#LOOP OVER ALL 8 RESPONSES: 1 through 8 = 'tablevals' vector
for (h in yearnum){
  for (i in numvar) {
    #WITHIN-IMPUTATION VARIANCE
    #POISSON DISTRIBUTION - mean of POISSON is also the variance
    v1 <- mice_imputed_data1.nat[h,i]
    v2 <- mice_imputed_data2.nat[h,i]
    v3 <- mice_imputed_data3.nat[h,i]
    v4 <- mice_imputed_data4.nat[h,i]
    v5 <- mice_imputed_data5.nat[h,i]
    w  <- sum(v1,v2,v3,v4,v5)/5
    #BETWEEN-IMPUTATION VARIANCE
    b  <- (((mice_imputed_data1.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data2.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data3.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data4.nat[h,i] - final.parest.imp[h,i])^2) + 
             ((mice_imputed_data5.nat[h,i] - final.parest.imp[h,i])^2))/4
    #TOTAL VARIANCE, then calculate standard error
    s  <- sqrt(w + (1 + (1/5))*b)
    #final CI
    natCI <- c(final.parest.imp[h,i] - (s*1.96), final.parest.imp[h,i] + (s*1.96))
    assign(paste0("natCI.",year[h],".",i),natCI,envir = .GlobalEnv)
  }
}

#append confidence intervals for ease of output REQUIRES UPDATING!!!!
natCI.2018all <- rbind(natCI.2018.1,natCI.2018.2,natCI.2018.3,natCI.2018.4,
                       natCI.2018.5,natCI.2018.6,natCI.2018.7,natCI.2018.8)
natCI.2019all <- rbind(natCI.2019.1,natCI.2019.2,natCI.2019.3,natCI.2019.4,
                       natCI.2019.5,natCI.2019.6,natCI.2019.7,natCI.2019.8)
natCI.2020all <- rbind(natCI.2020.1,natCI.2020.2,natCI.2020.3,natCI.2020.4,
                       natCI.2020.5,natCI.2020.6,natCI.2020.7,natCI.2020.8)
natCI.2021all <- rbind(natCI.2021.1,natCI.2021.2,natCI.2021.3,natCI.2021.4,
                       natCI.2021.5,natCI.2021.6,natCI.2021.7,natCI.2021.8)
rownames(natCI.2018all) <- numvar
rownames(natCI.2019all) <- numvar
rownames(natCI.2020all) <- numvar
rownames(natCI.2021all) <- numvar

#manipulate estimates for merging with CIs
for (i in year) {
  natest <- as.data.frame(t(as.matrix(final.parest.imp[which(year==i),]))) %>%
    slice(numvar)
  rownames(natest) <- numvar
  colnames(natest) <- "Estimate"
  assign(paste0("nat.",i,"all"),natest,envir = .GlobalEnv)
}
