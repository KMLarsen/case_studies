source("Income_analysis.R")

makeBuckets <- function (data){
  # make a US v. non US
  data$united.states <- ifelse(data$native.country == 'United-States', TRUE, FALSE)
  data$native.country <- NULL
  
  # workclass: Private v. non Private
  data$private.sector <- ifelse(data$workclass == 'Private', TRUE, FALSE)
  data$workclass <- NULL
  
  return(data)
}

fixUnknowns <- function (data){
  # native country: mark '?' as United States
  data$native.country <- ifelse(data$native.country == '?', 'United-States', as.character(data$native.country))

  # workclass: mark '?' as Private
  data$workclass <- ifelse(data$workclass == '?', 'Private', as.character(data$workclass))
  
  # occupation: for hs grad >, for some college >, for other > Prof-specialty
  data$occupation <- ifelse(data$occupation == '?',
                            ( ifelse(data$education == 'HS-grad','Craft-repair',
                                   (ifelse(data$education == 'Some-college', 'Adm-clerical','Prof-specialty'))))
                                   , as.character(data$occupation))
  # make back to factor
  data$occupation <- factor(data$occupation)
  
  return(data)
}

# divide into 2/3 training, 1/3 test from random 3rd index
main <- function (){
  # read in data
  data <- readData()
  
  # create test and train indices
  n <- nrow(data)
  test_ind <- seq(0, n, 3)
  train_ind <- setdiff( seq(0, n, 1), test_ind )
  
  # scale data
  numericCols <- names(Filter(is.numeric, data))
  for (col in numericCols){
    data[,col] <- scale(data[,col])[,]
  }
  
  # remove features without clear distinctions of income level (see histogram)
  data$fnlwgt <- NULL
  
  # deal with missing data
  data <- fixUnknowns(data)
  
  # bucket categories: country (US v. non US)
  data <- makeBuckets(data)
  
  # split test and train...REMOVE LAST COLUMN
  test <- data[test_ind, -12]
  train <- data[train_ind, -12]
  test_labels <- data[test_ind, 'yearly.income']
  train_labels <- data[train_ind, 'yearly.income']
  
  # save to .dat file
  write.table(test, file="test.dat", row.names=FALSE, sep="\t", quote=FALSE)
  write.table(train, file="train.dat", row.names=FALSE, sep="\t", quote=FALSE)
  write.table(test_labels, file="test_labels.dat", row.names=FALSE, sep="\t", quote=FALSE)
  write.table(train_labels, file="train_labels.dat", row.names=FALSE, sep="\t", quote=FALSE)
}

