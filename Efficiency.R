library(ggplot2)
library(sparklyr)
library(scales)

setwd("/home/arcs/Oct14/")
getwd()
newdata <- read.csv("14Oct2017CPUvsWallTime.csv", header = T, sep=",")
names(newdata)
str(newdata)
class(newdata)

##Convertion to numeric value
newdata[,3] <- as.numeric(as.character(newdata[,3])) #RemoteWallClockTime

#Cleanseing data by removing NA rows
newdata1 = na.omit(newdata)


newdata1$CPUTime <- newdata1$RemoteSysCpu + newdata1$RemoteUserCpu 
newdata1$WallTime <- newdata1$RemoteWallClockTime - newdata1$CumulativeSuspensionTime

newdata1$Efficiency <- newdata1$CPUTime/ newdata1$WallTime

str(newdata1)

#Graph of CPU Time VS WallTime

graph1 <- ggplot(newdata1, aes(x = newdata1$Efficiency)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 )
graph1 + labs(title= "CPU Time vs WallTime", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")




#Graph of CPU Time vs Wall Time where y=log10(x)

graph2 <- ggplot(newdata1, aes(x = newdata1$Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0)) #+
#scale_x_continuous( xlim = c(0,1.2), bandwidth = 0.1)
graph2 + labs(title= "CPU Time vs WallTime", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")

#Extraction of interesting data into 2 dataframe

efficiency_grt_1.2 <- subset(newdata1, newdata1$Efficiency>1.2) #Extract Jobs with efficiency > 1.2
efficiency_less_1.2 <- subset(newdata1, newdata1$Efficiency <= 1.2) #Extract Jobs with efficiency <= 1.2

#Graph of CPU Time VS WallTime for jobs with efficiency <=1.2 in log scale
graph3 <- ggplot(efficiency_less_1.2, aes(x = Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0))
graph3 + labs(title= "CPU Time vs WallTime for jobs with efficiency <=1.2, Bin size = 50", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")

#Graph of CPU Time VS WallTime for jobs with efficiency <=1.2
graph3.1 <- ggplot(efficiency_less_1.2, aes(x = Efficiency)) +
  geom_histogram( color = "Black", fill = "Red", bins = 50, alpha = 0.7 )
graph3.1 + labs(title= "CPU Time vs WallTime for jobs with efficiency <=1.2, Bin size = 50", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")

#Graph of CPU Time VS WallTime for jobs with efficiency > 1.2
graph3.1 <- ggplot(efficiency_grt_1.2, aes(x = Efficiency)) +
  geom_histogram( color = "Black", fill = "Red", bins = 50, alpha = 0.7 )
graph3.1 + labs(title= "CPU Time vs WallTime for jobs with efficiency > 1.2, Bin size = 50", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")


#Graph of CPU Time VS WallTime for jobs with efficiency > 1.2
graph5 <- ggplot(efficiency_grt_1.2, aes(x = Efficiency)) +
  geom_histogram(color = "Black", fill = "darkgreen", bins = 50 ) #+
#scale_y_continuous(trans="log10", expand=c(0,0))
graph5 + labs(title= "CPU Time vs WallTime for jobs for jobs with efficiency < 1.2, Bin = 50", x= "Effiency = CPU Time/WallTime", y = "Number of Jobs")

#Set of jobs with CPUTime <=30
cpu_time_less30_S <- subset(efficiency_less_1.2, CPUTime<=30)

#Plot of jobs with CPU time < 30seconds
plot(cpu_time_less30_S$CPUTime)

#Histogram of CPU Time for jobs with CPU time <= 30 Second
graph6 <- ggplot(cpu_time_less30_S, aes(x = CPUTime)) +
  geom_histogram(color = "Black", fill = "darkgreen", bins = 30 ) +
  scale_y_continuous(trans="log10", expand=c(0,0))
graph6 + labs(title= "CPU Time", x= "CPU Time (in Seconds)", y = "Number of Jobs")

#Density plot of CPU Time for jobs with Efficiency <= 1.2
ggplot(efficiency_less_1.2, aes(x=CPUTime)) +
  stat_density(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_y_continuous(trans="log10", expand=c(0,0)) +
  scale_x_continuous( expand=c(0,0)) +
  labs(title = "CPU Time - Density Function", x = "CPU Time (in Seconds)", y = "log(Number of Jobs)") +
  theme_bw()

#Density plot of Wall Time for jobs with Efficiency <= 1.2
ggplot(efficiency_less_1.2, aes(x=WallTime)) +
  stat_density(aes(y=..count..), color="black", fill="blue", alpha=0.3) +
  scale_y_continuous( trans="log10", expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  labs(title = "Wall Time - Density Function ", x = "WallTime (in Seconds)", y = "log(Number of Jobs)") +
  theme_classic()

