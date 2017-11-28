library(ggplot2)
library(scales)

setwd("/home/arcs/Oct14/DataCSV")
getwd()
newdata <- read.csv("14Oct2017EfficiencyMem1.csv", header = T, sep=",")

###################################################################
############# Studying the structure of Data ######################
###################################################################
names(newdata)
str(newdata)
summary(newdata)

###################################################################
############# Conversion to numeric values ########################
###################################################################
newdata[,"RemoteWallClockTime"] <- as.numeric(as.character(newdata[,"RemoteWallClockTime"])) #RemoteWallClockTime
newdata[, "ExitCode"] <- as.numeric(as.character(newdata[, "ExitCode"]))
newdata[, "MemoryUsage"] <- as.numeric(as.character(newdata[, "MemoryUsage"]))


###################################################################
##################### Data Cleansing ##############################
###################################################################

unique(newdata$JobUniverse)
unique(newdata$Remote_JobUniverse)
unique(newdata$ExitCode)
newdata2 <- subset(newdata, newdata$ExitCode == 0)
unique(newdata2$ExitCode)
unique(newdata2$JobUniverse)
unique(newdata2$Remote_JobUniverse)

###################################################################
############### Computation of efficiency #########################
###################################################################

newdata2$CPUTime <- newdata2$RemoteSysCpu + newdata2$RemoteUserCpu 
newdata2$WallTime <- newdata2$RemoteWallClockTime - newdata2$CumulativeSuspensionTime
newdata2$Efficiency <- newdata2$CPUTime/ newdata2$WallTime



#Cleanseing data by removing NA rows
newdata2 <- subset(newdata2, newdata2$Efficiency != "NA")
#newdata3 <- subset(newdata2, select =  c(CPUTime, WallTime, Efficiency))
#newdata3 <- na.omit(newdata3)
summary(newdata2)
###########################################################################
str(newdata2)
unique(newdata2$MemoryUsage)
unique(newdata2$RequestMemory)
jobMem1900 <- subset(newdata2, newdata2$RequestMemory == 1900)
hist(jobMem1900$MemoryUsage)
plot(jobMem1900$MemoryUsage, jobMem1900$Efficiency)
TotalCPUTime_JobMem1900 <- sum(as.numeric(JobMem1900$CPUTime))
TotalWallTime_JobMem1900 <- sum(JobMem1900$WallTime)
TotalCPUTime_JobMem1900
TotalWallTime_JobMem1900
CumulativeEfficiency_JobMem1900 <- TotalCPUTime_less_1.2/TotalWallTime_less_1.2
CumulativeEfficiency_JobMem1900




hist(newdata2$MemoryUsage)
plot(newdata2$MemoryUsage, newdata2$Efficiency)
#Graph of CPU Time VS WallTime

graph1 <- ggplot(newdata3, aes(x = Efficiency)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 ) #+
#scale_x_continuous(bandwidth = 0.1 )
graph1 + labs(title= "CPU Time vs WallTime", x= "Efficiency = CPU Time/WallTime", y = "Number of Jobs")


#Graph of CPU Time vs Wall Time where y=log10(x)

graph2 <- ggplot(newdata3, aes(x = Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0)) #+
#scale_x_continuous( xlim = c(0,1.2), bandwidth = 0.1)
graph2 + labs(title= "CPU Time vs WallTime", x= "Efficiency = CPU Time/WallTime", y = "Number of Jobs")


######################################################################
##################### Classification of dataset ######################
#####################   into 2 groups based of  ######################
#####################     efficiency            ######################
######################################################################

efficiency_grt_1.2 <- subset(newdata3, Efficiency > 1.2) #Extract Jobs with efficiency > 1.2
efficiency_less_1.2 <- subset(newdata3, Efficiency <= 1.2) #Extract Jobs with efficiency <= 1.2

######################################################################
################ Compute Overall efficiency of    ####################
#####################   of different classes    ######################
######################################################################

TotalCPUTime_less_1.2 <- sum(as.numeric(efficiency_less_1.2$CPUTime))
TotalWallTime_less_1.2 <- sum(efficiency_less_1.2$WallTime)
TotalCPUTime_less_1.2
TotalWallTime_less_1.2
CumulativeEfficiency_less_1.2 <- TotalCPUTime_less_1.2/TotalWallTime_less_1.2
CumulativeEfficiency_less_1.2

TotalCPUTime_grt_1.2 <- sum(as.numeric(efficiency_grt_1.2$CPUTime))
TotalWallTime_grt_1.2 <- sum(efficiency_grt_1.2$WallTime)
TotalCPUTime_grt_1.2
TotalWallTime_grt_1.2
CumulativeEfficiency_grt_1.2 <- TotalCPUTime_grt_1.2/TotalWallTime_grt_1.2
CumulativeEfficiency_grt_1.2

TotalCPUTime <- sum(as.numeric(newdata3$CPUTime))
TotalWallTime <- sum(newdata3$WallTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency


###########################################################################################
######################################   SUMMARY    #######################################
###########################################################################################
#############################  Total no of jobs : 257561 ##################################
#########################  Number of jobs after cleansing : 113703 ########################
##############  Number of jobs with efficiency <= 1.2 (Class I) : 111589 ##################
###############  Number of jobs with efficiency > 1.2 (Class II) : 2017 ###################
#############################  Overall Efficiency : 3.041 #################################
###################### Overall Efficiency of Class I jobs : 0.873 #########################
######################  Overall Efficiency of Class I jobs : 5.302 ########################
###########################################################################################



#Graph of CPU Time for jobs with efficiency >1.2

png(filename = "/home/arcs/Oct14/RCodes/Plots/CPUTime.png")


graph3 <- ggplot(efficiency_grt_1.2, aes(x = CPUTime)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 ) #+
#scale_x_continuous(bandwidth = 0.1 )
graph3 + labs(title= "CPU Time", x= "CPU Time", y = "Number of Jobs")
dev.off()

#Graph of Wall Time for jobs with efficiency >1.2

graph4 <- ggplot(efficiency_grt_1.2, aes(x = WallTime)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 ) #+
#scale_x_continuous(bandwidth = 0.1 )
graph4 + labs(title= "Wall Time", x= "Wall Time", y = "Number of Jobs")


plot(efficiency_grt_1.2$CPUTime, efficiency_grt_1.2$WallTime, alpha = .5)
abline(a = 0, b = 1)
efficiency_grt_1.2$Diff <- efficiency_grt_1.2$CPUTime - efficiency_grt_1.2$WallTime

graph5 <- ggplot(efficiency_grt_1.2, aes(x = Diff)) +
  geom_histogram( color = "Black", fill = "Pink", bins = 50 ) #+
#scale_x_continuous(bandwidth = 0.1 )
graph5 + labs(title= "bla bla", x= "CPUTime - WallTime", y = "Number of Jobs")


Error <- sum(efficiency_grt_1.2$Diff)
