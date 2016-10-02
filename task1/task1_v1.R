# Author: Surya Seetharaman
# Task1 : Getting familiar with R

# importing datasets
data.x <- read.csv("X.csv")
data.y <- read.csv("Y.csv")

# disabling scientific notation in R.
options(scipen = 999)
# Compute the following statistics for each component of X and Y: 
# mean, maximum, minimum, 25th percentile, 90th percentile, 
# standard deviation, and variance.
apply(data.x, 2, mean)
apply(data.x, 2, max)
apply(data.x, 2, min)
apply(data.x, 2, sd)
apply(data.x, 2, var)
quantile(data.x$TimeStamp, c(.25, .90))


