data.x <- read.csv("X.csv")
print(data.x)
#to find the maximum of each component in X.csv
max(data.x$TimeStamp)
max(data.x$all_..idle)
max(data.x$X..memused)
max(data.x$proc.s)
max(data.x$cswch.s)
max(data.x$file.nr)
max(data.x$sum_intr.s)
max(data.x$ldavg.1)
max(data.x$tcpsck)
max(data.x$pgfree.s)

data.y <- read.csv("Y.csv")
print(data.y)
#to find the maximum of each component in Y.csv
max(data.y$TimeStamp)
max(data.y$DispFrames)

#to find the minimum of each component in X.csv
min(data.x$TimeStamp)
min(data.x$all_..idle)
min(data.x$X..memused)
min(data.x$proc.s)
min(data.x$cswch.s)
min(data.x$file.nr)
min(data.x$sum_intr.s)
min(data.x$ldavg.1)
min(data.x$tcpsck)
min(data.x$pgfree.s)

#to find the minimum of each component in Y.csv
min(data.y$TimeStamp)
min(data.y$DispFrames)

#to find the mean of each component in X.csv
mean(data.x$TimeStamp)
mean(data.x$all_..idle)
mean(data.x$X..memused)
mean(data.x$proc.s)
mean(data.x$cswch.s)
mean(data.x$file.nr)
mean(data.x$sum_intr.s)
mean(data.x$ldavg.1)
mean(data.x$tcpsck)
mean(data.x$pgfree.s)

#to find the mean of each component in Y.csv
mean(data.y$TimeStamp)
mean(data.y$DispFrames)

#to find the variance of each component in X.csv
TS.var <- var(data.x$TimeStamp)
var(data.x$all_..idle)
var(data.x$X..memused)
var(data.x$proc.s)
var(data.x$cswch.s)
var(data.x$file.nr)
var(data.x$sum_intr.s)
var(data.x$ldavg.1)
var(data.x$tcpsck)
var(data.x$pgfree.s)

#to find the varianceof each component in Y.csv
var(data.y$TimeStamp)
var(data.y$DispFrames)

#to find the standard deviation of each component in X.csv
sqrt(TS.var)
#sqrt()

#to find the percentile of tcpsck
socket <-data.x$tcpsck
quantile(socket, c(.25, .90))

#to find the number of rows whose memory usage is larget than 80%
sum(data.x$X..memused > 80)

#the average number of used TCP sockets for observations with more than 18000 interrupts/sec;
datax.socket <- data.x[data.x$sum_intr.s > 18000,]
print(datax.socket$tcpsck)
mean(datax.socket$tcpsck)
