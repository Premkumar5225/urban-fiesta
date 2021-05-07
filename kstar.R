require("class")
setwd("C://Users//PK//Documents//data set")
cancer <- read.csv("lung21.csv")
test <- read.csv(file="C://Users//PK//Documents//test1.csv", header=TRUE, sep=",")
testdata<-data.frame(test)

 # load cancer Dataset
str(cancer)
summary(cancer)
head(cancer)
set.seed(99) # required to reproduce the results
rnum<- sample(rep(1:1000)) # randomly generate numbers from 1 to 150
rnum
cancer<- cancer[rnum,] #randomize "cancer" dataset
head(cancer)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

cancer.new<- as.data.frame(lapply(cancer[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)],normalize))

head(cancer.new)
cancer.train<- cancer.new[1:600,]
cancer.train.target<- cancer[1:600,25]
cancer.test<- cancer.new[601:1000,]
cancer.test.target<- cancer[601:1000,25]
summary(cancer.new)
anyNA(cancer.new)
model1<- knn(train=cancer.train, test=cancer.test, cl=cancer.train.target, k=31,prob=TRUE)

xtab<-table(cancer.test.target, model1)
library(caret)
#It is used to calculate the accuracy, precision, recall and F-Measure. 
library(rminer)
confusionMatrix(xtab)

