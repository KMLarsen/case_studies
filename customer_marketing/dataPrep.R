# Data preparation file

fixMissing <- function (data){
  # set schooling as the mode
  schoolMode <- names(which.max(table(data$schooling)))
  data[which( data$schooling == 'unknown'),"schooling"] <- schoolMode
  data[is.na(data$schooling),"schooling"] <- schoolMode
  data$schooling <- factor(data$schooling)
  
  # set default as mode
  defaultMode <- names(which.max(table(data$default)))
  data[which(data$default == 'unknown'),"default"] <- defaultMode
  data$default <- factor(data$default)
  
  # set loan as mode
  loanMode <- names(which.max(table(data$loan)))
  data[which(data$loan == 'unknown'),"loan"] <- loanMode
  data$loan <- factor(data$loan)
  
  # set profession as mode
  professionMode <- names(which.max(table(data$profession)))
  data[which(data$profession == 'unknown'),"profession"] <- professionMode
  data$profession <- factor(data$profession)
  
  # set marital as mode
  maritalMode <- names(which.max(table(data$marital)))
  data[which(data$marital == 'unknown'),"marital"] <- maritalMode
  data$marital <- factor(data$marital)
  
  # set day_of_week as mode
  dayMode <- names(which.max(table(data$day_of_week)))
  data[is.na(data$day_of_week),"day_of_week"] <- dayMode
  data$day_of_week <- factor(data$day_of_week)
  
  # set age as average
  data[is.na(data$custAge),"custAge"] <- mean(data$custAge,na.rm=TRUE)
  
  # set housing as mode
  housingMode <- names(which.max(table(data$housing)))
  data[which(data$housing == 'unknown'),"housing"] <- housingMode
  data$housing <- factor(data$housing)
  
  return (data)
  
}

fixSkew <- function(data){
  # log campaign
  data$campaign <- log(data$campaign)
  
  # log nremployed
  data$nr.employed <- log(data$nr.employed)
  
  return (data)
}

combinePrevious <- function(data){
  # combine pdays, previous, pmonths, pastEmail
  
  # new column pContact from pdays and pmonths (use pdays mean of 6.18  as cutoff)
  data$pContact<- ifelse(data$pdays == 999, 'None',
                         ifelse(data$pdays < 6.18, 'Recent','Older'))
  data$pContact <- factor(data$pContact)
  
  # new column pFreq from previous and pastEmail
  data$pFreq<- ifelse(data$previous == 0, 'None',
                         ifelse(data$previous < 1.28, 'Low','High'))
  data$pFreq <- factor(data$pFreq)
  
  return (data)
}

combineRates <- function(data){
  # combine highly correlation rate variables
  # emp.var.rate, cons.price.idx, nr.employed, euribor3m
  
  # make euribor as the floor for categories
  data$euribor3mFlr <- floor(data$euribor3m)
  

}

makeBuckets <- function(data){
  # bucket age
  data$ageBkt <- cut(data$custAge,breaks=quantile(data$custAge,probs=seq(0,1, by=0.25), na.rm=TRUE),include.lowest=TRUE)
  levels(data$ageBkt) <- c('Q1','Q2','Q3','Q4')
  
  # bucket season
  data$season <- as.factor( 
                ifelse(data$month %in% c('mar','apr','may'),'Spring',
                                    ifelse(data$month %in% c('jun','jul','aug'),'Summer',
                                           ifelse(data$month %in% c('sep','oct','nov'), 'Fall','Winter'))))
  
  return (data)

}

setResponse <- function(data){
  data$responded <- ifelse(data$responded == 'yes',1,0)
  
  return (data)
}