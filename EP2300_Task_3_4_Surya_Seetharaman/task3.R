# Author: Surya Seetharaman
# Task3 : Estimating SLA Conformance and Violation from Device Statistics

#!/usr/bin/env Rscript

#importing required libraries
library(ggplot2)
options(digits = 12)

# disabling scientific notation in R.
#options(scipen = 999)

#to get the dataset files as command line arguments as mentioned in the specifications
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=2) {
  stop("The two arguments must be supplied (both the input files).n", call.=FALSE)
}

X <- args[1]
Y <- args[2]
#print(X)

# importing datasets
data.x <- read.csv(file = X)
data.y <- read.csv(file = Y)

#data.x <- read.csv("X.csv")
#data.y <- read.csv("Y.csv")


#validation-set technique
data <- data.x[1:10]
data["DispFrames"] <- data.y$DispFrames
data["Binary.DispFrames"] <- ifelse(data["DispFrames"]>=18, 1, 0);
    
#print(data);

# proportion of subset data
prob <- 0.7 
set.seed(3579)

# training and test data set 
training.s <- sample (1:nrow(data), round(prob*nrow(data),0))
data.train <- data[training.s,]
data.test <- data[-training.s,]
#print(data.train)
#print(data.test)

#Training sets Preparation - from the above training set with 2520 observations, create five training
#sets by selecting uniformly at random 50 observations, 500 observations, 1000 observations, 1500
#observations, and 2520 observations (which is the original set).
set.seed(3579)

training.set <- function(x, y) {
  c(x[sample(1:nrow(x), round(y*nrow(x),0)),])
}

data.50.train <- training.set(data.train, 0.0198412698)
data.500.train <- training.set(data.train, 0.1984126984)
data.1000.train <- training.set(data.train, 0.3968253968)
data.1500.train <- training.set(data.train, 0.5952380952)
data.2520.train <- training.set(data.train, 1)

#a) Classifier Training - use logistic regression to train five classifiers C 1 , ..., C 5 , 
#one for each training set. Make a table showing the coefficients (Θ) of the classifiers;
C1 <- glm(data.50.train$Binary.DispFrames ~., data = data.50.train[2:10], family=binomial)
coefficients(C1)

C2 <- glm(data.500.train$Binary.DispFrames ~., data = data.500.train[2:10], family="binomial" )
coefficients(C2)

C3 <- glm(data.1000.train$Binary.DispFrames ~., data = data.1000.train[2:10], family="binomial")
coefficients(C3)

C4 <- glm(data.1500.train$Binary.DispFrames ~., data = data.1500.train[2:10], family="binomial")
coefficients(C4)

C5 <- glm(data.2520.train$Binary.DispFrames ~., data = data.2520.train[2:10], family = "binomial")
coefficients(C5)

#b) Time to Train the Classifiers C 1 , ..., C 5 - measure the execution time in milliseconds to 
#train each classifier;
time.50 = system.time(glm(data.50.train$Binary.DispFrames ~., data = data.50.train[2:10], family=binomial))
time.500 = system.time(glm(data.500.train$Binary.DispFrames ~., data = data.500.train[2:10], family="binomial" ))
time.1000 = system.time(glm(data.1000.train$Binary.DispFrames ~., data = data.1000.train[2:10], family="binomial"))
time.1500 = system.time(glm(data.1500.train$Binary.DispFrames ~., data = data.1500.train[2:10], family="binomial"))
time.2520 = system.time(glm(data.2520.train$Binary.DispFrames ~., data = data.2520.train[2:10], family = "binomial"))
print(time.50)
print(time.500)
print(time.1000)
print(time.1500)
print(time.2520)
time <- c(time.50["elapsed"]*1000, time.500["elapsed"]*1000, time.1000["elapsed"]*1000, time.1500["elapsed"]*1000, time.2520["elapsed"]*1000)
print(time)

#c) Accuracy of the Classifiers C 1 , ..., C 5 - Compute the classification error (ERR) on the test set
#for each classifier. Consider a classifier as accurate when ERR < 15%;

model.ERR <- function(x) {
  prediction <- predict.glm(x, data.test[2:10], type="response")
  #print(prediction)
  data.test["answer"] = prediction
  TP=0
  TN=0
  
  for(index in 1:1080){
    if(data.test$answer[index] >= 0.5 & data.test$Binary.DispFrames[index] == 1){
      TP <- TP + 1;
    }
    if(data.test$answer[index] < 0.5 & data.test$Binary.DispFrames[index] == 0){
      TN <- TN + 1;
    }
  }
  ERR <- 1 -((TP+TN)/1080)
  print(ERR)
  c(ERR)
}

result.50 <- model.ERR(C1)
result.500 <- model.ERR(C2)
result.1000 <- model.ERR(C3)
result.1500 <- model.ERR(C4)
result.2520 <- model.ERR(C5)
err <- c(result.50, result.500, result.1000, result.1500, result.2520)
print(err)


#d) Produce a plot that shows both the Classification Error (ERR) on the test set and Training Time
#for each of the five classifiers C 1 , ..., C 5 . Use two y-axes to include both curves in 
#a single plot;
x.axis <- c(50,500,1000,1500,2520)

jpeg('1d.jpg', width=600,height=500)
par(mar = c(5, 4, 4, 4) + 0.3) 
plot(x.axis,err,type="b",col="red", las = 1, ylim = c(0,0.20), xlab = "size of training set", ylab = "", main="ERR and Training Time for all five classifiers" )
mtext("classification error(ERR)",side=2,line=3)
par(new=TRUE)
plot(x.axis,time,type="b",col="blue", xaxt="n",yaxt="n", las = 1, ylim = c(0,50), xlab = "", ylab = "")
axis(4)
mtext("training time(in milli seconds)" ,side=4,line=2)
legend("bottomright", col=c("red","blue"),lty=1,legend=c("ERR","training time(ms)"))
dev.off()

#e) Using the test set produce a time series plot that shows on #the y-axis the observed video frame rate.
prediction1 <- predict.glm(C1, data.test[2:10], type="response")
prediction2 <- predict.glm(C2, data.test[2:10], type="response")
prediction3 <- predict.glm(C3, data.test[2:10], type="response")
prediction4 <- predict.glm(C4, data.test[2:10], type="response")
prediction5 <- predict.glm(C5, data.test[2:10], type="response")

x = data.test$TimeStamp
y1 = data.test$DispFrames
y2 <- array(0, dim=c(1, 1080))
y3 <- array(0, dim=c(1, 1080))

val = which.min(err)
print(val)
if(val == 1)
{
  classifier = prediction1
}
if(val == 2)
{
  classifier = prediction2
}
if(val == 3)
{
  classifier = prediction3
}
if(val == 4)
{
  classifier = prediction4
}
if(val == 5)
{
  classifier = prediction5
}
#print(classifier)
for(index in 1:1080){
  if((classifier[index] >= 0.5 & data.test$Binary.DispFrames[index] == 1)||(classifier[index] < 0.5 & data.test$Binary.DispFrames[index] == 0)){
    y2[index] = data.test$DispFrames[index]
    y3[index] = NA
  }
  else{
    y2[index] = NA
    y3[index] = data.test$DispFrames[index]
  }
}
#print(y2)
jpeg('1e.jpg', width=600,height=500)
plot(x,y1,type="p",  col="black", ylim=c(0,30), main="Time series plot that shows the observed video frame rate", xlab="TimeStamp", ylab= "Video Frame Rates")
lines(x,y2,type="p", col="green", ylim=c(0,30))
lines(x,y3,type="p", col="red", ylim=c(0,30))
legend("bottomright", pch = c(21, 21), c("incorrectly predicted dispframe values","correctly predicted dispframe values"), col = c("red","green"))  
dev.off()




