#install.packages("pROC")
library(pROC)
library(nnet)
library(car)

# split indices for cross validation on models
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K)  #approximate size of each part
  r<-n-m*K  
  I<-sample(n,n)  #random reordering of the indices
  Ind<-list()  #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart]  #indices for kth part of data
  }
  Ind
}


multiReg <- function(train){
  # only train on those who have responded
  train <- train[which(train$responded == 'yes'),]
  train$responded <- NULL
  
  # drop columns with no factors variations, refactor others
  train$default <- NULL
  train$schooling <- factor(train$schooling)
  train$profession <- relevel(train$profession, ref = "management")

  # Test with Step: step wise produces all individual variables as predictors
  
  # don.lm0 <- lm(profit ~ 1, data= train)
  # don.lmFull <- lm(profit ~ . + .:., data = train)
  # don.lmStep <- step(don.lm0, scope = list( lower = don.lm0, upper = don.lmFull), direction = "both", trace = 0)
  
  # vif(don.lmStep)
  # remove high vif of euribor3m
  fit2 <- lm(formula = profit ~ loan + profession + marital + euribor3m:season, data = train)
  fit3 <- lm(formula = profit ~ loan + poutcome + season + ageBkt + pContact + pFreq, data = train)
  
  # view residual
  #plot(y, fit2$residuals)
  #plot(y, don.lmFull$residuals)
  #plot(y, don.lmStep$residuals)
  
  # run cross validation
  # multiRegCV(train)
  
  # final model
  final <- lm(formula = profit ~ loan + profession + season + 
                ageBkt + euribor3m:ageBkt + euribor3m:month, data = train)
  #summary(final)
  #vif(final)
  
  # return best model
  return (final)
}

multiRegCV <- function(train){
  Nrep<-3 #number of replicates of CV
  K<-10  #K-fold CV on each replicate
  n.models = 4 #number of different models to fit
  n=nrow(train)
  yhat=matrix(0,n,n.models)
  MSE<-matrix(0,Nrep,n.models)
  
  y<-as.numeric(train$profit)
  train$responded <- NULL
  
  for (j in 1:Nrep) {
    Ind<-CVInd(n,K)
    for (k in 1:K) {
      # null model
      out<- lm(profit ~ 1,train[-Ind[[k]],])
      yhat[Ind[[k]],1]<- predict(out,train[Ind[[k]],])
      
      # full model
      out<- lm(profit ~ .,train[-Ind[[k]],])
      yhat[Ind[[k]],2]<- predict(out,train[Ind[[k]],])
      
      # model from step wise
      out<- lm(formula = profit ~ loan + profession + season + 
                 ageBkt + euribor3m:ageBkt + euribor3m:month, data = train[-Ind[[k]],])
      yhat[Ind[[k]],3]<- predict(out,train[Ind[[k]],])
      
      # manual input model
      out<- lm(formula = profit ~ loan + profession + marital + euribor3m:season + ageBkt, data = train[-Ind[[k]],])
      yhat[Ind[[k]],4]<- predict(out,train[Ind[[k]],])
      
      
    } #end of k loop
    MSE[j,]=apply(yhat,2,function(x) sum((y-x)^2))/n
  } #end of j loop
  
  # view results of cross validation
  MSE
  MSEAve<- apply(MSE,2,mean); MSEAve #averaged mean square CV error
  MSEsd <- apply(MSE,2,sd); MSEsd   #SD of mean square CV error
  r2<-1-MSEAve/var(y); r2  #CV r^2
  
  return (r2)
}

logModel <- function(train){
  train$profit <- NULL

  # Test with Step: step wise produces all individual variables as predictors
  null <- glm(responded ~ 1, family = binomial(link="logit"), data= train)
  full <- glm(responded ~ ., family = binomial(link="logit"), data= train)
  logStep <- step(null, scope=list(lower=null, upper=full), direction="both", trace = 0)
  #summary(logStep)

  # misclass = sum(y != yhat)/length(y); misclass
  
  # define own fit model
  logFit <- glm(responded ~ .,family=binomial(link='logit'),data=train)
  
  # run cross validation > shows the step function as the best model on cv misclass rate

  return (logStep)
}

logModelCV <- function(train){
  ##Now use multiple reps of CV to compare logistic models
  Nrep<-3 #number of replicates of CV
  K<-10  #K-fold CV on each replicate
  n.models = 4 #number of different models to fit
  n=nrow(train)
  yhat=matrix(NA,n,n.models)
  misclass<-matrix(0,Nrep,n.models)
  
  # set y as the yes/no with 1/0 encoding
  y<-as.numeric(train$responded)
  train$profit <- NULL
  
  train$respondedBin <- ifelse(train$responded == 'yes',1,0)
  train$responded <- NULL
  train$default <- NULL # will likely be missing the multiple levels in 10 fold cv
  
  # remove record of schooling where illiterate is a level
  train <- train [which(train$schooling != 'illiterate'),] 
  
  threshold <- 0.4
  
  for (j in 1:Nrep) {
    Ind<-CVInd(n,K)
    for (k in 1:K) {
      # full model
      out<- glm(respondedBin ~ .,family= binomial, train[-Ind[[k]],])
      probs <- predict(out,newdata = train[Ind[[k]],], type = "response")
      classes <- sapply(probs, function(x) ifelse(x > threshold, 1, 0))
      yhat[Ind[[k]],1]<- classes
      
      # null model
      out<- glm(respondedBin ~ 1,family= binomial(link="logit"), train[-Ind[[k]],])
      probs <- predict(out,train[Ind[[k]],] , type = "response")
      classes <- sapply(probs, function(x) ifelse(x > threshold, 1, 0))
      yhat[Ind[[k]],2]<- classes
      
      # model from step wise
      out<- glm(respondedBin ~ nr.employed + poutcome + month + contact + 
                  pmonths + emp.var.rate + cons.price.idx + cons.conf.idx + 
                  pastEmail + profession + campaign, family= binomial, data = train[-Ind[[k]],], trace = 0)
      probs <- predict(out,train[Ind[[k]],] , type = "response")
      classes <- sapply(probs, function(x) ifelse(x > threshold, 1, 0))
      yhat[Ind[[k]],3]<- classes
      
      # manual model
      out<- glm(respondedBin ~ nr.employed + poutcome + season + contact + 
                  pContact + emp.var.rate + cons.price.idx + cons.conf.idx + 
                  pFreq + profession + campaign, family= binomial, train[-Ind[[k]],])
      probs <- predict(out,train[Ind[[k]],] , type = "response")
      classes <- sapply(probs, function(x) ifelse(x > threshold, 1, 0))
      yhat[Ind[[k]],4]<- classes
      
    } #end of k loop
    misclass[j,]=apply(yhat,2,function(x) sum(y != x)/length(y) ) #training misclassification rate
  } #end of j loop
  
  # view results of cross validation
  misclass
  misclassAve<- apply(misclass,2,mean); misclassAve #averaged misclass rate
  misclassSd <- apply(misclass, 2, sd); misclassSd  #st dev misclass rate

  return (misclass)
}

respondProb <- function(model, test){
  yhatProb <- predict(newdata = test, model, type="response")
  # yhat <- predict(data = test, model, type="class")
  
  # set threshold
  pstar<-.4
  yhatClass <- sapply(yhatProb,function(x) ifelse(x > pstar, 1, 0))
  yhatProb2 <- sapply(yhatProb,function(x) ifelse(x > pstar, yhatProb, 0))
  
  return (yhatClass)
}

predProfit <- function(model, test){
  predProfit <- predict(newdata=test, model)
  return (predProfit)
}



