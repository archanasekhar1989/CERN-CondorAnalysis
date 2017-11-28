setwd("/home/arcs/Oct14/")
getwd()
mydata <- read.csv("14Oct2017.csv", header = T, sep=",")
names(mydata)
mydata[,10] <- suppressWarnings(as.numeric(as.character(mydata[,10])))
hist(mydata$MemoryUsage, xlab = "Memory Used by the job", freq = F)
library(ggplot2)
ggplot(mydata, aes(x = mydata$MemoryUsage)) +
  geom_histogram()

## plot runtime and cpu time in the same plot

mydata[,1] <- suppressWarnings(as.numeric(as.character(mydata[,1]))) #JobStartTime
mydata[,6] <- suppressWarnings(as.numeric(as.character(mydata[,6]))) #CompletionTime
mydata$RunTime <- mydata$CompletionDate - mydata$JobStartDate #RunTime = JobStartTime - CompletionTime


plot(mydata$RunTime,
     data=mydata$RunTime)
points(mydata$CommittedTime, col="red",
       data=mydata$CommittedTime)
legend(1975, 400000,
       c("RunTime", "CPU Time"), title="Wall time VS CPU Time",
       col=c("black", "red"),
       pch=c(1, 1))

