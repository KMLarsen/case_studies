setwd("/Users/katelarsen/Documents/Career/Northwestern/Uptake/Larsen_Case")
source("EDA.R")
source("dataPrep.R")
source("model.R")

' 
=================
1) load data
================= 
'
addr<- "/Users/katelarsen/Documents/Career/Northwestern/Uptake/uptakecasestudydocuments"
setwd(addr)
training <- read.csv("training.csv",header = TRUE)
testing <- read.csv("testingCandidate.csv",header = TRUE)

' 
=================
2) clean data

- Remove ID
- Fix missing data
- Log skewed data
================= 
'
training$id <- NULL
training <- fixMissing(training)
training <- fixSkew(training)
training <- combinePrevious(training)
training <- makeBuckets(training)

' 
=================
3) build models

- build logistic (predict respond / not respond)
- build multiple regression (predicted profit)
================= 
'
# get logistic models
logModel <- logModel(training)

# get estimated size of profit
regModel <- multiReg(training)

' 
=================
4) test models

- apply trained models to test data
- adjust data as previously done on training data
- A: predict yes / no (1/0) from the logistic model
- B: predict amount of profit for customer
- multiply A * B
================= 
'
# preprocess training data
test <- testing
test <- fixMissing(test)
test <- fixSkew(test)
test <- combinePrevious(test)
test <- makeBuckets(test)

# get probabilities from log model
respondBin <- respondProb(logModel, test)

# retrieve predicted profit from lm
predictedProfit <- predProfit(regModel, test)
ex
# get multiplied of responded * how much
expected<-predictedProfit * respondBin

# number of positive predicted donors:
length(which(expected == 0)) # 30888 not responded
length(which(expected > 30)) / nrow(test) # 0.05908953 positive profits
length(which(expected > 0 & expected < 30)) / nrow(test) # 0.003095599 responded but neg profit

# donor IDs to target
predictedID <- which(expected > 30)
test[predictedID,]

# if you target those with positive predicted profits, total profit expected:
predAve <- sum( expected[expected > 30] ) / length( expected[expected > 30] ) ; predAve

# compare to historical training percentage of profits
nrow(training[which(training$profit > 30),]) / nrow(training)
histAve <- sum (training[which(training$profit > 30),"profit"]) / nrow( training[which(training$profit > 30),]); histAve

# export to file to update testing csv
toMarket <- ifelse(expected > 30, 1, 0)
write.csv(tomarket,file="toMarket.csv")
