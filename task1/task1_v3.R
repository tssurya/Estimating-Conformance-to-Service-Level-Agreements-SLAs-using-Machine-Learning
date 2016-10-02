# Author: Surya Seetharaman
# Task1 : Getting familiar with R

#!/usr/bin/env Rscript

#importing required libraries
library(ggplot2)

# disabling scientific notation in R.
options(scipen = 999)

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
x.axis = data.x$TimeStamp
y1 = (data.x$all_..idle)
y2 = (data.x$X..memused)



jpeg('Time_Series_plot.jpg', width=750)
g <- ggplot(data.x, aes(x.axis))
g <- g + geom_line(aes(y=y1, colour="red"))
g <- g + geom_line(aes(y=y2, colour="blue"))
g <- g + ylab("percentage") + xlab("TimeStamp")
g <- g + ggtitle("Time series of percentage of idle CPU and of used memory")
g <- g + scale_colour_manual("Legend", values = c("red", "blue"), labels = c("Memory Used", "Idle CPU"))
g
dev.off()

# (b) Density plots of idle CPU and of used memory.
jpeg('Idle_CPU_density_plot.jpg', width=600,height=500)
d1 <- density(data.x$all_..idle) # returns the density data
plot(d1, type = "o", col = "blue", main="Density plot of idle CPU") # plots the results 
polygon(d1, col="red") 
dev.off()
jpeg('Memory_Used_density_plot.jpg', width=600,height=500)
d2 <- density(data.x$X..memused)
plot(d2, type = "o", col = "blue", main="Density plot of used memory")
polygon(d2, col="red") 
dev.off()



