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
data["Binary.DispFrames"] <- ifelse(data["DispFrames"]>=18, 1, 0)

# proportion of subset data
prob <- 0.7 
set.seed(777)

# training and test data set 
training.s <- sample (1:nrow(data), round(prob*nrow(data),0))
data.train <- data[training.s,]
data.test <- data[-training.s,]
#print(data.train)
#print(data.test)

#To build the first classifier, extend the linear regression function developed in Task II with a check on the
#output, i.e., the frame rate. If the frame rate for a given X is above the SLA threshold, then the Y label of
#the classifier is set to conformance, otherwise to violation. We call this classifier C_lin .
linear.model <- lm(data.train$DispFrames ~., data = data.train[2:10], method = "qr" )
ans <- predict(linear.model, data.test[2:10])
data.test["linear.answer"] = ans
#coefficients(linear.model)
data.test["linear.DispFrames"] <- ifelse(data.test["linear.answer"]>=18, 1, 0)

#raining sets Preparation - from the above training set with 2520 observations, create five training
#sets by selecting uniformly at random 50 observations, 500 observations, 1000 observations, 1500
#observations, and 2520 observations
set.seed(777)

training.set <- function(x, y) {
  c(x[sample(1:nrow(x), round(y*nrow(x),0)),])
}

data.50.train <- training.set(data.train, 0.0198412698)
data.500.train <- training.set(data.train, 0.1984126984)
data.1000.train <- training.set(data.train, 0.3968253968)
data.1500.train <- training.set(data.train, 0.5952380952)
data.2520.train <- training.set(data.train, 1)

#a) Classifier Training - train five classifiers C lin1 , ..., C lin5 , one for each training set.
linear.model.50 = lm(data.50.train$DispFrames ~., data = data.50.train[2:10], method = "qr" )
coefficients(linear.model.50)

linear.model.500 = lm(data.500.train$DispFrames ~., data = data.500.train[2:10], method = "qr" )
coefficients(linear.model.500)

linear.model.1000 = lm(data.1000.train$DispFrames ~., data = data.1000.train[2:10], method = "qr" )
coefficients(linear.model.1000)

linear.model.1500 = lm(data.1500.train$DispFrames ~., data = data.1500.train[2:10], method = "qr" )
coefficients(linear.model.1500)

linear.model.2520 = lm(data.2520.train$DispFrames ~., data = data.2520.train[2:10], method = "qr" )
coefficients(linear.model.2520)


#a) Do the same for the classifiers C log1 , ..., C log5 .
C1 <- glm(data.50.train$Binary.DispFrames ~., data = data.50.train[2:10], family=binomial)
coefficients(C1)

C2 <- glm(data.500.train$Binary.DispFrames ~., data = data.500.train[2:10], family=binomial )
coefficients(C2)

C3 <- glm(data.1000.train$Binary.DispFrames ~., data = data.1000.train[2:10], family=binomial)
coefficients(C3)

C4 <- glm(data.1500.train$Binary.DispFrames ~., data = data.1500.train[2:10], family=binomial)
coefficients(C4)

C5 <- glm(data.2520.train$Binary.DispFrames ~., data = data.2520.train[2:10], family = binomial)
coefficients(C5)

#b) Accuracy of the Classifiers - Compute the classification error (ERR) for C lin1 , ..., C lin5 and
#C log1 , ..., C log5 . Consider a classifier as accurate when ERR < 15%;
log.model.ERR <- function(x) {
  prediction <- predict.glm(x, data.test[2:10], type="response")
  #print(prediction)
  data.test["Clog"] = prediction
  TP=0
  TN=0
  
  for(index in 1:1080){
    if(data.test$Clog[index] >= 0.5 & data.test$Binary.DispFrames[index] == 1){
      TP <- TP + 1;
    }
    if(data.test$Clog[index] < 0.5 & data.test$Binary.DispFrames[index] == 0){
      TN <- TN + 1;
    }
  }
  ERR <- 1 -((TP+TN)/1080)
  print(ERR)
  c(ERR)
}

log.result.50 <- log.model.ERR(C1)
log.result.500 <- log.model.ERR(C2)
log.result.1000 <- log.model.ERR(C3)
log.result.1500 <- log.model.ERR(C4)
log.result.2520 <- log.model.ERR(C5)
log.err <- c(log.result.50, log.result.500, log.result.1000, log.result.1500, log.result.2520)
print(log.err)

lin.model.ERR <- function(x) {
  prediction <- predict(x, data.test[2:10], type="response")
  #print(prediction)
  data.test["Clin"] <- ifelse(prediction>=18, 1, 0)
  
  TP=0
  TN=0
  
  for(index in 1:1080){
    if(data.test$Clin[index] == 1 & data.test$Binary.DispFrames[index] == 1){
      TP <- TP + 1;
    }
    if(data.test$Clin[index] == 0 & data.test$Binary.DispFrames[index] == 0){
      TN <- TN + 1;
    }
  }
  ERR <- 1 -((TP+TN)/1080)
  print(ERR)
  c(ERR)
}

lin.result.50 <- lin.model.ERR(linear.model.50)
lin.result.500 <- lin.model.ERR(linear.model.500)
lin.result.1000 <- lin.model.ERR(linear.model.1000)
lin.result.1500 <- lin.model.ERR(linear.model.1500)
lin.result.2520 <- lin.model.ERR(linear.model.2520)
lin.err <- c(lin.result.50, lin.result.500, lin.result.1000, lin.result.1500, lin.result.2520)
print(lin.err)

#Produce a plot that shows on the y-axis the classification error in the test set, and on the x-axis
#the size of the training set. The errors of the classifiers C lin1 , ..., C lin5 form a curve on this plot,
#the errors of the classifiers C log1 , ..., C log5 form a second curve;

x.axis <- c(50,500,1000,1500,2520)

jpeg('task4.1c.jpg', width=600,height=500)
plot(x.axis,lin.err,type="b",  col="red", ylim=c(0,0.20), main="ERR for all the ten classifiers", xlab="size of training set", ylab= "ERR for all the classifiers")
lines(x.axis,log.err,type="b", col="blue", ylim=c(0,30))
legend("bottomright", pch = c(21, 21), c("ERR for Clin","ERR for Clog"), col = c("red","blue"))  
dev.off()

