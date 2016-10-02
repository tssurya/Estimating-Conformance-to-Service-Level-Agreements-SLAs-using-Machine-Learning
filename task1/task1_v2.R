# Author: Surya Seetharaman
# Task1 : Getting familiar with R

#importing required libraries
library(ggplot2)

# disabling scientific notation in R.
options(scipen = 999)

# importing datasets
data.x <- read.csv("/home/linux/EP2300_Management_of_Networks_and_Networked_Systems/Project/task1/X.csv")
data.y <- read.csv("/home/linux/EP2300_Management_of_Networks_and_Networked_Systems/Project/task1/Y.csv")

# Compute the following statistics for each component of X and Y: 
# mean, maximum, minimum, 25th percentile, 90th percentile, 
# standard deviation, and variance.
multi.fun <- function(x) {
  c(mean = mean(x), minimum = min(x), maximum = max(x), percentile = quantile(x, c(.25)), percentile = quantile(x, c(.90)), standard.deviation = sd(x), variance = var(x))
}
sapply(data.x, multi.fun)
sapply(data.y, multi.fun)

# Compute the following quantities:
# (a) the number of observations with memory usage larger than 80%;
sum(data.x$X..memused > 80)

# (b) the average number of used TCP sockets for observations with more than 18000 interrupts/sec;
datax.socket <- data.x[data.x$sum_intr.s > 18000,]
mean(datax.socket$tcpsck)

# (c) the minimum memory utilization for observations with CPU idle time lower than 20%.
datax.mem <- data.x[data.x$all_..idle < 0.2,]
min(datax.mem$X..memused)

# Produce the following plots:
# (a) Time series of percentage of idle CPU and of used memory (both in a single plot);
x = data.x$TimeStamp
y1 = (data.x$all_..idle)
y2 = (data.x$X..memused)

g <- ggplot(data.x, aes(x))
g <- g + geom_line(aes(y=y1, colour="red"))
g <- g + geom_line(aes(y=y2, colour="blue"))
g <- g + ylab("percentage") + xlab("TimeStamp")
g <- g + ggtitle("Time series of percentage of idle CPU and of used memory")
g <- g + scale_colour_manual("Legend", values = c("red", "blue"), labels = c("Memory Used", "Idle CPU"))
g

# (b) Density plots of idle CPU and of used memory.
d1 <- density(data.x$all_..idle) # returns the density data
plot(d1, main="Density plot of idle CPU") # plots the results 
d2 <- density(data.x$X..memused)
plot(d2, main="Density plot of used memory")

#plot(c(1409265000,1409268000), c(0,100), type="n", xlab="x", ylab="y") 
plot(x, y1, type = "l", col="red") 
lines(x, y2, col="blue") 
#legend("topright", inset=.05, cex = 1, title="Legend", c("Cosinus","Sinus"), horiz=TRUE, lty=c(1,1), lwd=c(2,2), col=c("red","blue"), bg="grey96")  