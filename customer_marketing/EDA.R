library(ggplot2)
library(reshape)

# function to plot multiple histograms to files
plotHist <- function (data, file){
  # plot 4 separate figures for readability
  n <- length(data)
  j <- floor(n/4)
  i <- 1
  
  # loop through variables, subsetting by 4
  while ( i <= n) {
    # define index as current and current + 4
    k <- i + j
    
    # if not at end of the list, use current+4, else use end n
    if (k <= n){
      # melt to variable name and values
      gg <- melt(data[,c(i:k)])
      
      ggplot(gg, aes(x=value, fill=variable)) +
        geom_histogram(binwidth=1)+
        facet_grid(variable~., scales="free")
    }
    else{
      k <- n
      gg <- melt(data[,c(i:k)])
      
      ggplot(gg, aes(x=value, fill=variable)) +
        geom_histogram(binwidth=1)+
        facet_grid(variable~., scales="free")
    }
    
    # save to current directory
    fileName <- paste0(paste0(file,i),".png")
    ggsave(fileName)
    
    # increment to next column of the +4 range
    i <- k + 1
  }
}

'
=============
Load Data
=============
'
addr<- "/Users/katelarsen/Documents/Career/Northwestern/Uptake/uptakecasestudydocuments"
setwd(addr)

training <- read.csv("training.csv",header = TRUE)
testing <- read.csv("testingCandidate.csv",header = TRUE)

'
=============
Explore Data: Missing Values

Profit: Some are negative
============= 
'
summary(training)
summary(training[training$pdays < 999,"pdays"])
summary(training$previous)
summary(training$pmonths)

# negative profits
nrow(training[which(training$profit < 0),])

'
=============
Correlations: 
pdays, pmonths, previous
previous, pastemail
emp.var.rate, cons.price.idx, nr.employed, euribor3m
=============
'
numericCols <- sapply(training, is.numeric)
numericCols['responded'] <- FALSE
numericCols['profit'] <- FALSE

cov <-cor(training[,numericCols]); cov
pairs(cov, cex=.5, pch=16)


'
=============
Skewness
For those with heavy skew, examine histogram
=============
'
library(e1071)
apply(training[,numericCols], 2, function(x) skewness(x))

# histograms
hist(training$pdays)
hist(training$profession)
#hist(training$campaign)
hist(training$pdays)
hist(training[training$pdays < 999,"pdays"])
hist(training$previous)
hist(training$cons.conf.idx)
hist(training$euribor3m)
hist(training[training$pmonths < 999,"pdays"])
summary(training[training$pastEmail > 0,"pastEmail"])
hist(training$pastEmail)

# generate all numeric histograms and save to file
plotHist(training[,numericCols],'trainingHist')

'
=============
Explore "previous" columns
=============
'
hist(training$previous)
summary(training[training$previous > 0,'previous'])
summary(training[training$pdays < 999,'pdays'])
summary(training[training$pmonths < 999,'pmonths'])
length(training[training$pastEmail > 0,'pastEmail'])
length(training[training$previous > 0,'previous'])


'
=============
Explore market columns
=============
'
hist(training$cons.conf.idx)
summary(training[training$cons.conf.idx,'cons.conf.idx'])
hist(training$cons.price.idx)
summary(training[training$cons.conf.idx,'cons.price.idx'])

summary(training[training$pdays < 999,'pdays'])
summary(training[training$pmonths < 999,'pmonths'])
length(training[training$pastEmail > 0,'pastEmail'])
length(training[training$previous > 0,'previous'])
