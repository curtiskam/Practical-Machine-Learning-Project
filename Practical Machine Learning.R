## load libraries

library(caret)
library(randomForest)
library(e1071)
library(rattle)
library(knitr)

## download data files

trainFile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainFileName <- "trainingdata.csv"

testFile <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testFileName <- "testdata.csv"

download.file(trainFile, destfile=trainFileName)
download.file(testFile, destfile=testFileName)

## Read data sets into data frames
trainData <- read.csv("trainingdata.csv")
testData <- read.csv("testdata.csv")

##Remove NAs
#trainData <- trainData[, colSums(is.na(trainData)) == 0] 
#testData <- testData[, colSums(is.na(trainData)) == 0] 

##Get rid of columns with little effect on accelerometer
classe <- trainData$classe
trainFix <- grepl("^X|timestamp|window", names(trainData))
trainData <- trainData[, !trainFix]
trainData <- trainData[, sapply(trainData, is.numeric)]
trainData$classe <- classe

classe <- testData$classe
testFix <- grepl("^X|timestamp|window", names(testData))
testData <- testData[, !testFix]
testData <- testData[, sapply(testData, is.numeric)]
testData$classe <- classe

## Partition TrainData to create a training set and a test set within the training data

set.seed(333) # For reproducibile purpose
inTrain <- createDataPartition(trainData$classe, p=0.70, list=F)
trainSet <- trainData[inTrain, ]
testSet <- trainData[-inTrain, ]

## Train Decsion Tree
modFitRpart <-  train(classe ~ ., data=trainSet, method="rpart")

## Train Random Forest Tree
modFitRF <- train(classe ~ ., data=trainSet, method="rf")

##Print the Models##

predictRpart <- predict(modFitRpart, testData)
predictRpart

predictRF <- predict(modFitRF, testData)
predictRF