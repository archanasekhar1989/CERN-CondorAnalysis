library(ggplot2)
library(scales)
library(data.table)
setwd("/home/arcs/Oct14/DataCSV")
getwd()
#newdata <- read.csv("Oct2017Efficiency.csv", header = T, sep=",")
newdata <- fread("Oct2017Efficiency.csv")
class(newdata)
###################################################################
############# Studying the structure of Data ######################
###################################################################
names(newdata)
str(newdata)
summary(newdata)

###################################################################
############# Conversion to numeric values ########################
###################################################################
#newdata[,"RemoteWallClockTime"] <- as.numeric(as.character(newdata[,"RemoteWallClockTime"])) #RemoteWallClockTime
#newdata[, "ExitCode"] <- as.numeric(as.character(newdata[, "ExitCode"]))
#newdata[, "MemoryUsage"] <- as.numeric(as.character(newdata[, "MemoryUsage"]))



newdata[,"RemoteWallClockTime"] <- as.numeric(unlist(newdata[,"RemoteWallClockTime"])) #RemoteWallClockTime
newdata[, "ExitCode"] <- as.numeric(unlist(newdata[, "ExitCode"]))
newdata[, "MemoryUsage"] <- as.numeric(unlist(newdata[, "MemoryUsage"]))
newdata[, "MATCH_HEPSPEC"] <- as.numeric(unlist(newdata[, "MATCH_HEPSPEC"]))
newdata[, "MATCH_TotalCpus"] <- as.numeric(unlist(newdata[, "MATCH_TotalCpus"]))



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
newdata2$WallTime <- newdata2$RemoteWallClockTime #- newdata2$CumulativeSuspensionTime
newdata2$HEPSPEC_TotalCpus <- newdata2$MATCH_HEPSPEC/ newdata2$MATCH_TotalCpus
newdata2$NWallTime <- newdata2$WallTime * newdata2$RequestCpus * newdata2$HEPSPEC_TotalCpus
newdata2$NCPUTime <- newdata2$CPUTime * newdata2$HEPSPEC_TotalCpus
newdata2$NEfficiency <- newdata2$NCPUTime/ newdata2$NWallTime



#Cleanseing data by removing NA rows
newdata2 <- subset(newdata2, newdata2$NEfficiency != "NA")
#newdata3 <- subset(newdata2, select =  c(CPUTime, WallTime, Efficiency))
#newdata3 <- na.omit(newdata3)
summary(newdata2)

TotalCPUTime <- sum(as.numeric(newdata2$NCPUTime))
TotalWallTime <- sum(newdata2$NWallTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency


###########################################################################
str(newdata2)
unique(newdata2$MemoryUsage)
unique(newdata2$RequestMemory)


#################### Jobs with memory request 1900 ######################################
jobMem1900 <- subset(newdata2, newdata2$RequestMemory == 1900)
hist(jobMem1900$MemoryUsage)
plot(jobMem1900$MemoryUsage, jobMem1900$NEfficiency)
TotalCPUTime_JobMem1900 <- sum(as.numeric(jobMem1900$NCPUTime))
TotalWallTime_JobMem1900 <- sum(jobMem1900$NWallTime)
TotalCPUTime_JobMem1900
TotalWallTime_JobMem1900
CumulativeEfficiency_JobMem1900 <- TotalCPUTime_JobMem1900/TotalWallTime_JobMem1900
CumulativeEfficiency_JobMem1900


#################### Jobs with memory request 18000 ######################################
jobMem18000 <- subset(newdata2, newdata2$RequestMemory == 18000)
hist(jobMem18000$MemoryUsage)
plot(jobMem18000$MemoryUsage, jobMem18000$NEfficiency)
TotalCPUTime_JobMem18000 <- sum(as.numeric(jobMem18000$NCPUTime))
TotalWallTime_JobMem18000 <- sum(jobMem18000$NWallTime)
TotalCPUTime_JobMem18000
TotalWallTime_JobMem18000
CumulativeEfficiency_JobMem18000 <- TotalCPUTime_JobMem18000/TotalWallTime_JobMem18000
CumulativeEfficiency_JobMem18000

#################### Jobs with memory request 0 ######################################
jobMem0 <- subset(newdata2, newdata2$RequestMemory == 0)
hist(jobMem0$MemoryUsage)
plot(jobMem0$MemoryUsage, jobMem0$Efficiency)
TotalCPUTime_JobMem0 <- sum(as.numeric(jobMem0$CPUTime))
TotalWallTime_JobMem0 <- sum(jobMem0$WallTime)
TotalCPUTime_JobMem0
TotalWallTime_JobMem0
CumulativeEfficiency_JobMem0 <- TotalCPUTime_JobMem0/TotalWallTime_JobMem0
CumulativeEfficiency_JobMem0

#################### Jobs with memory request 4000 ######################################
jobMem4000 <- subset(newdata2, newdata2$RequestMemory == 4000)
hist(jobMem4000$MemoryUsage)
plot(jobMem4000$MemoryUsage, jobMem4000$Efficiency, pch=16)
TotalCPUTime_JobMem4000 <- sum(as.numeric(jobMem4000$CPUTime))
TotalWallTime_JobMem4000 <- sum(jobMem4000$WallTime)
TotalCPUTime_JobMem4000
TotalWallTime_JobMem4000
CumulativeEfficiency_JobMem4000 <- TotalCPUTime_JobMem4000/TotalWallTime_JobMem4000
CumulativeEfficiency_JobMem4000


#################### Jobs with memory request 2000 ######################################
jobMem2000 <- subset(newdata2, newdata2$RequestMemory == 2000)
hist(jobMem2000$MemoryUsage)
plot(jobMem2000$MemoryUsage, jobMem2000$Efficiency, pch=16)
TotalCPUTime_JobMem2000 <- sum(as.numeric(jobMem2000$CPUTime))
TotalWallTime_JobMem2000 <- sum(jobMem2000$WallTime)
TotalCPUTime_JobMem2000
TotalWallTime_JobMem2000
CumulativeEfficiency_JobMem2000 <- TotalCPUTime_JobMem2000/TotalWallTime_JobMem2000
CumulativeEfficiency_JobMem2000


#################### Jobs with memory request 16000 ######################################
jobMem16000 <- subset(newdata2, newdata2$RequestMemory == 16000)
hist(jobMem16000$MemoryUsage)
plot(jobMem16000$MemoryUsage, jobMem16000$Efficiency, pch=16)
TotalCPUTime_JobMem16000 <- sum(as.numeric(jobMem16000$CPUTime))
TotalWallTime_JobMem16000 <- sum(jobMem16000$WallTime)
TotalCPUTime_JobMem16000
TotalWallTime_JobMem16000
CumulativeEfficiency_JobMem16000 <- TotalCPUTime_JobMem16000/TotalWallTime_JobMem16000
CumulativeEfficiency_JobMem16000

#################### Jobs with memory request 3700 ######################################
jobMem3700 <- subset(newdata2, newdata2$RequestMemory == 3700)
hist(jobMem3700$MemoryUsage)
plot(jobMem3700$MemoryUsage, jobMem3700$Efficiency, pch=16)
TotalCPUTime_JobMem3700 <- sum(as.numeric(jobMem3700$CPUTime))
TotalWallTime_JobMem3700 <- sum(jobMem3700$WallTime)
TotalCPUTime_JobMem3700
TotalWallTime_JobMem3700
CumulativeEfficiency_JobMem3700 <- TotalCPUTime_JobMem3700/TotalWallTime_JobMem3700
CumulativeEfficiency_JobMem3700

#################### Jobs with memory request 2130 ######################################
jobMem2130 <- subset(newdata2, newdata2$RequestMemory == 2130)
hist(jobMem2130$MemoryUsage)
plot(jobMem2130$MemoryUsage, jobMem2130$Efficiency, pch=16)
TotalCPUTime_JobMem2130 <- sum(as.numeric(jobMem2130$CPUTime))
TotalWallTime_JobMem2130 <- sum(jobMem2130$WallTime)
TotalCPUTime_JobMem2130
TotalWallTime_JobMem2130
CumulativeEfficiency_JobMem2130 <- TotalCPUTime_JobMem2130/TotalWallTime_JobMem2130
CumulativeEfficiency_JobMem2130

#################### Jobs with memory request 3200 ######################################
jobMem3200 <- subset(newdata2, newdata2$RequestMemory == 3200)
hist(jobMem3200$MemoryUsage)
plot(jobMem3200$MemoryUsage, jobMem3200$Efficiency, pch=16)
TotalCPUTime_JobMem3200 <- sum(as.numeric(jobMem3200$CPUTime))
TotalWallTime_JobMem3200 <- sum(jobMem3200$WallTime)
TotalCPUTime_JobMem3200
TotalWallTime_JobMem3200
CumulativeEfficiency_JobMem3200 <- TotalCPUTime_JobMem3200/TotalWallTime_JobMem3200
CumulativeEfficiency_JobMem3200

TotalCPUTime <- sum(as.numeric(newdata2$CPUTime))
TotalWallTime <- sum(newdata2$WallTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency



