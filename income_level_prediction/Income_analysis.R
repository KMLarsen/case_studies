#install.packages("gridExtra")
library(plotly)
library(gridExtra)
source("Income_plots.R")

readData <- function(){
  addr <- '/Users/katelarsen/Documents/School/Northwestern/Career/Noodle'
  file <- 'income_prediction.txt'
  setwd(addr)
  data <- read.delim(file, header = TRUE, sep = ",")
  
  # remove all whitespace for factors
  categCols <- names(Filter(is.factor, data))
  for (col in categCols){
    data[col] <- as.data.frame(apply(data[col],2,function(x)gsub('\\s+', '',x)))
  }

  return (data)
}

agebucketTable <- function(data){
  # max age is 90
  maxAge <- max(data$age)
  
  # add an ageBucket column to each data point
  data$ageBucket <- cut(data$age, seq(0,90,10), include.lowest=TRUE, right = FALSE)
  
  #check buckets
  head(data[,c("age", "ageBucket")])
  
  # calcualte mean hours per week
  meanHrs <- aggregate(data$hours.per.week, by=list(data$ageBucket), FUN=mean)
  colnames(meanHrs) <- c("Age Bucket", "Mean Hours / Week")
  
  # calculate median education number
  medEduc <- aggregate(data$education.num, by=list(data$ageBucket), FUN=median)
  colnames(medEduc) <- c("Age Bucket", "Median Education")
  
  tableAgg <- merge(meanHrs, medEduc, by = "Age Bucket")
  
  print(tableAgg)
  grid.table(tableAgg)
}

occupationTable <- function(data){
  # replace ? as Unknown
  data$occupation <- ifelse(data$occupation == '?', 'Unknown', as.character(data$occupation))
  
  # divide into male and female subsets
  dataF <- data[which(data$sex == 'Female'),]
  dataM <- data[which(data$sex == 'Male'),]
  
  # calculate median age by occupation for each subset
  medAgeFemale <- aggregate(dataF$age, by=list(dataF$occupation), FUN=median)
  colnames(medAgeFemale) <- c("Occupation", "Median Female Age")

  medAgeMale <- aggregate(dataM$age, by=list(dataM$occupation), FUN=median)
  colnames(medAgeMale) <- c("Occupation", "Median Male Age")
  
  tableAgg <- merge(medAgeFemale, medAgeMale, by = "Occupation", all.y = TRUE)
  
  print(tableAgg)
  grid.table(tableAgg)

}

countUnknown <- function(data, categCols){
  categUnknown = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(categUnknown) = c("Column", "Number of Unknown", "Percent Unknown")
  
  n <- nrow(data)
  data$Unknown <- 0
  
  i <- 1
  for (col in categCols){
    countUnknown <- length( data[which(data[,col] == '?'),col])
    pctUnknown <- countUnknown / n
    
    newrow <- c(col, countUnknown, round(pctUnknown,3) )
    categUnknown[i,] <- newrow
    i <- i + 1
    data$Unknown <- ifelse(data[,col] == '?', 1, data$Unknown)
  }
  
  totalUnknown <- nrow(data[which(data$Unknown == 1),])
  newrow <- c("Total with Overlap", totalUnknown, round(totalUnknown / n,3) )
  categUnknown[i,] <- newrow

  # for unknown occupation levels, look at common occupations by education level 
  educFreq<- count(data[which(data$education == 'HS-grad'),'occupation'])
  colnames(educFreq) <- c("Occupation for HS-grad","Freq")
  grid.table(educFreq[order(-educFreq$Freq),])
  
  educFreq<- count(data[which(data$education == 'Some-college'),'occupation'])
  colnames(educFreq) <- c("Occupation for Some-college","Freq")
  grid.table(educFreq[order(-educFreq$Freq),])
  
  return (categUnknown)
}

levelAnalysis <- function (data, categCols){
  # for each column, print number of levels,
  # for highest level counts, print the distribution
  library(plyr)
  levelAnalysis = data.frame(matrix(ncol = 2, nrow = 0))
  
  colnames(levelAnalysis) = c("Column", "Number of Levels")
  
  i <- 1
  for (col in categCols){
    numLevels <- length(levels(data[,col]))
    levels <- levels(data[,col])
    
    newrow <- c(col, numLevels)
    levelAnalysis[i,] <- newrow
    i <- i + 1
  }

  print(levelAnalysis)
  grid.table(levelAnalysis)
  
  # for the highest level counts, print the distribution of levels
  countries <- data.frame(table(data[,c("native.country")]))
  colnames(countries) <- c("Country","Freq")
  countries <- countries[order(-countries$Freq),]
  grid.table(countries[1:10,])
  
  return (levelAnalysis)
}

categoricalStats <- function(data, categCols){
  # count the number of unknown values in categorical columns
  categUnknown <- countUnknown(data, categCols)
  print(categUnknown)
  grid.table(categUnknown)
  
  # for each column, check the number of levels and distribution within each
  levelAnalysis <- levelAnalysis(data, categCols)
  
}

numericStats <- function(data, numericCols){
  #library(matrixStats)
  library(e1071)

  # get summary statistics
  numStats = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(numStats) = c("Column", "Mean", "Median","Standard Deviation","Skewness")

  i <- 1
  for (col in numericCols){
    mean <- mean(data[,col])
    median <- median(data[,col])
    sd <- sd(data[,col])
    skew <- skewness(data[,col])
    
    newrow <- c(col, round(mean,3), median, round(sd,3), round(skew,3) )
    numStats[i,] <- newrow
    
    i <- i + 1
  }
  print(numStats)
  grid.table(numStats)
  
  # check correlation - if high correlation, then dont want to use logistic regression
  grid.table( round(cor(data[,numericCols]),3) )
}

visualData <- function(data, numericCols, categCols){
  # Numeric columns
  numericDistr(data)
  
  # view boxplots with scaled data
  for (col in numericCols){
    data[,col] <- scale(data[,col])[,]
  }
  boxPlots(data)
  
  # Categorical
  categDistr(data)
  unknownData <- countUnknown(data, categCols)
  n <- nrow(data)
  unknownData(unknownData, n)
  
}

main <- function(){
  # read in data
  data = readData()
  
  # Q1 Column Stats
  # get statistics for numeric columns first, then categorical columns
  numericCols <- names(Filter(is.numeric, data))
  numericStats(data, numericCols)
  
  categCols <- names(Filter(is.object, data))
  categoricalStats(data, categCols)
  
  # Q2 Visuals
  visualData(data, numericCols, categCols)
  
  # Q3 Tables
  agebucketTable(data)
  occupationTable(data)
}