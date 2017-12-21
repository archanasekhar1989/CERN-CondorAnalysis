setwd("remote: HTTP Basic: Access denied
fatal: Authentication failed for 'https://:@gitlab.cern.ch:8443/aarun/Condor_analysis.git/")
getwd()
newdata <- read.csv("14Oct2017CPUvsWallTime.csv", header = T, sep=",")
names(newdata)
class(newdata)
dim(newdata)#newdata
str(newdata)
library(dplyr)
glimpse(newdata)
##Convertion to numeric value
#newdata[,1] <- suppressWarnings(as.numeric(as.character(newdata[,1]))) #RemoteSysCpu
#newdata[,2] <- suppressWarnings(as.numeric(as.character(newdata[,2]))) #RemoteUserCpu
newdata[,3] <- as.numeric(as.character(newdata[,3])) #RemoteWallClockTime
#newdata[,4] <- suppressWarnings(as.numeric(as.character(newdata[,4]))) #CumulativeSuspensionTime

newdata$CPUTime <- newdata$RemoteSysCpu + newdata$RemoteUserCpu 
newdata$WallTime <- newdata$RemoteWallClockTime - newdata$CumulativeSuspensionTime

newdata$Efficiency <- newdata$CPUTime/ newdata$WallTime
str(newdata)
#Cleanseing data by removing NA rows
newdata1 = na.omit(newdata)
str(newdata1)

TotalCPUTime <- sum(as.numeric(newdata1$CPUTime))
TotalWallTime <- sum(newdata1$RemoteWallClockTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency

library(ggplot2)
library(scales)

dim(newdata)
#Graph of CPU Time VS WallTime

graph1 <- ggplot(newdata1, aes(x = newdata1$Efficiency)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 ) #+
  #scale_x_continuous(bandwidth = 0.1 )
graph1 + labs(title= "CPU Time vs WallTime", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")




#Graph of CPU Time vs Wall Time where y=log10(x)

graph2 <- ggplot(newdata1, aes(x = newdata1$Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0)) #+
  #scale_x_continuous( xlim = c(0,1.2), bandwidth = 0.1)
graph2 + labs(title= "CPU Time vs WallTime", x= "Effiency = CPU Time/WallTime", y = "log10(Number of Jobs)")

efficiency_grt_1.2 <- subset(newdata1, newdata1$Efficiency>1.2) #Extract Jobs with efficiency > 1.2
efficiency_less_1.2 <- subset(newdata1, newdata1$Efficiency <= 1.2) #Extract Jobs with efficiency <= 1.2

TotalCPUTime <- sum(as.numeric(efficiency_less_1.2$CPUTime))
TotalWallTime <- sum(efficiency_less_1.2$RemoteWallClockTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency

TotalCPUTime <- sum(as.numeric(efficiency_grt_1.2$CPUTime))
TotalWallTime <- sum(efficiency_grt_1.2$RemoteWallClockTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency


#Graph of CPU Time VS WallTime for jobs with efficiency <=1.2
graph3 <- ggplot(efficiency_less_1.2, aes(x = efficiency_less_1.2$Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0))
graph3 + labs(title= "CPU Time vs WallTime for jobs with efficiency <=1.2, Bin size = 50", x= "Effiency = CPU Time/WallTime", y = "log10(Number of Jobs)")

#Graph of CPU Time VS WallTime for jobs with efficiency <=1.2
graph4 <- ggplot(efficiency_less_1.2, aes(x = efficiency_less_1.2$Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 100 ) #+
  #scale_y_continuous(trans="log10", expand=c(0,0))
graph4 + labs(title= "CPU Time vs WallTime for jobs with efficiency <=1.2, Bin size = 100", x= "Effiency = CPU Time/WallTime", y = "log10(Number of Jobs)")

#Graph of CPU Time VS WallTime for jobs with efficiency > 1.2
graph5 <- ggplot(efficiency_grt_1.2, aes(x = efficiency_grt_1.2$Efficiency)) +
  geom_histogram(color = "Black", fill = "darkgreen", bins = 50 ) #+
  #scale_y_continuous(trans="log10", expand=c(0,0))
graph5 + labs(title= "CPU Time vs WallTime for jobs for jobs with efficiency < 1.2, Bin = 50", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")

#Set of jobs with CPUTime <=30
cpu_time_less30_S <- subset(efficiency_grt, CPUTime<=30)

#Plot of jobs with CPU time < 30seconds
plot(cpu_time_less30_S$CPUTime)

#Histogram of CPU Time for jobs with CPU time <= 30 Second
graph6 <- ggplot(cpu_time_less30_S, aes(x = CPUTime)) +
  geom_histogram(color = "Black", fill = "darkgreen", bins = 30 ) +
  scale_y_continuous(trans="log10", expand=c(0,0))
graph6 + labs(title= "CPU Time", x= "CPU Time (in Seconds)", y = "log(Number of Jobs)")

#Density plot of CPU Time for jobs with Efficiency <= 1.2
ggplot(efficiency_grt, aes(x=CPUTime)) +
  stat_density(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_y_continuous(trans="log10", expand=c(0,0)) +
  scale_x_continuous( expand=c(0,0)) +
  labs(title = "CPU Time - Density Function", x = "CPU Time (in Seconds)", y = "log(Number of Jobs)") +
  theme_bw()

#Density plot of Wall Time for jobs with Efficiency <= 1.2
ggplot(efficiency_grt, aes(x=WallTime)) +
  stat_density(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_y_log10()
  #scale_y_continuous( trans="log10", expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "Wall Time - Density Function ", x = "WallTime (in Seconds)", y = "log(Number of Jobs)") +
  theme_classic()