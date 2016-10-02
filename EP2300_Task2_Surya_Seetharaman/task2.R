# Author: Surya Seetharaman
# Task2 : Estimating Service Metrics from Device Statistics

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

#validation-set technique
data <- data.x[1:10]
data["DispFrames"] <- data.y$DispFrames

# proportion of subset data
prob <- 0.7 
set.seed(1234)

# training and test data set 
training.s <- sample (1:nrow(data), round(prob*nrow(data),0))
data.train <- data[training.s,]
data.test <- data[-training.s,]
#print(data.train)
#print(data.test)

#Model Training - use linear regression to train a model M with the training set. 
#Provide the coefficients (Θ 1 , ..., Θ 9 ) of your model M .
linear.model <- lm(data.train$DispFrames ~., data = data.train[2:10], method = "qr" )
#summary(linear.model)
ans <- predict(linear.model, data.test[2:10])
data.test["answer"] = ans
coefficients(linear.model)


#Accuracy of Model M - compute the estimation error of M over the test P set. 
#We define the estimation error as the Normalized Mean Absolute Error.
#We consider an estimation accurate if NMAE < 15%.
y.i <- mean(data.test$DispFrames)
diff <- abs(c(data.test$DispFrames - ans))
accuracy <- (sum(diff)/1080)/y.i
print(accuracy)

#Produce a time series plot that shows the measurements and the model estimations for 
#the Video Frame Rate values in the test set.
x = data.test$TimeStamp
y1 = data.test$DispFrames
y2 = data.test$answer

jpeg('1c.jpg', width=600,height=500)
plot(x,y1,type="p",  col="red", ylim=c(0,30), main="Time series plot that shows the measurements \n and the model estimations for the Video Frame Rate values in the test set", xlab="TimeStamp", ylab= "Video Frame Rates")
lines(x,y2,type="p", col="blue", ylim=c(0,30))
legend("bottomright", pch = c(21, 21), c("Measured Video Frame Rate","Model Estimated Video Frame Rate"), col = c("red","blue"))  
dev.off()


#Produce a density plot for the Video Frame Rate values in the test set
jpeg('1d.jpg', width=600,height=500)
d1 <- density(data.test$DispFrames) # returns the density data
plot(d1, type = "o", col = "blue", main="Density plot of measured video frame rate values in the test set") # plots the results 
polygon(d1, col="red") 
dev.off()

#Produce a density plot for the NMAE values in the test set.
jpeg('1e.jpg', width=600,height=500)
d2 <- density(diff) # returns the density data
plot(d2, type = "o", col = "blue", main="Density plot of Absolute Errors in the test set") # plots the results 
polygon(d2, col="red") 
dev.off()

#Training sets Preparation - from the above training set with 2520 observations, 
#create five training sets by selecting uniformly at random 50 observations, 
#500 observations, 1000 observations, 1500 observations, and 2520 observations.

set.seed(1234)

training.set <- function(x, y) {
  c(x[sample(1:nrow(x), round(y*nrow(x),0)),])
}

data.50.train <- training.set(data.train, 0.0198412698)
data.500.train <- training.set(data.train, 0.1984126984)
data.1000.train <- training.set(data.train, 0.3968253968)
data.1500.train <- training.set(data.train, 0.5952380952)
data.2520.train <- training.set(data.train, 1)


#Model Training - use the same linear regression method as above to train five models 
#M 1 , ..., M 5 ,one for each training set. Provide a table with the coefficients of these models.

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

#Training Time of the Models - on your computer, measure the execution time 
#(in milliseconds) to train each of the five models.
time.50 = system.time(lm(data.50.train$DispFrames ~., data = data.50.train[2:10], method = "qr" ))
time.500 = system.time(lm(data.500.train$DispFrames ~., data = data.500.train[2:10], method = "qr" ))
time.1000 = system.time(lm(data.1000.train$DispFrames ~., data = data.1000.train[2:10], method = "qr" ))
time.1500 = system.time(lm(data.1500.train$DispFrames ~., data = data.1500.train[2:10], method = "qr" ))
time.2520 = system.time(lm(data.2520.train$DispFrames ~., data = data.2520.train[2:10], method = "qr" ))
print(time.50)
print(time.500)
print(time.1000)
print(time.1500)
print(time.2520)
time <- c(time.50["elapsed"]*1000, time.500["elapsed"]*1000, time.1000["elapsed"]*1000, time.1500["elapsed"]*1000, time.2520["elapsed"]*1000)
print(time)
#Accuracy of Models - compute the NMAE for each of the five models for the test set.

average.y = mean(data.test$DispFrames)

model.NMAE <- function(x) {
  prediction <- predict(x, data.test[2:10])
  difference <- abs(c(data.test$DispFrames - prediction))
  summation <- sum(difference)
  answer <- (summation/1080)/average.y
  c(answer)
}

result.50 <- model.NMAE(linear.model.50)
result.500 <- model.NMAE(linear.model.500)
result.1000 <- model.NMAE(linear.model.1000)
result.1500 <- model.NMAE(linear.model.1500)
result.2520 <- model.NMAE(linear.model.2520)
nmae <- c(result.50, result.500, result.1000, result.1500, result.2520)
print(nmae)

#Produce a plot that shows both the N M AE and Training Time for all models. 
#Use two y-axes, one on the left side and one on the right side, to include both curves 
#in a single plot.
x.axis <- c(50,500,1000,1500,2520)


jpeg('2d.jpg', width=600,height=500)
par(mar = c(5, 4, 4, 4) + 0.3) 
plot(x.axis,nmae,type="b",col="red", las = 1, ylim = c(0,0.2), xlab = "size of training set", ylab = "", main="NMAE and Training Time for all models" )
mtext("normalised mean absolute error",side=2,line=3)
par(new=TRUE)
plot(x.axis,time,type="b",col="blue", xaxt="n",yaxt="n", las = 1, ylim = c(0,8), xlab = "", ylab = "")
axis(4)
mtext("training time(in milli seconds)" ,side=4,line=2)
legend("topleft", col=c("red","blue"),lty=1,legend=c("NMAE","training time(ms)"))
dev.off()

