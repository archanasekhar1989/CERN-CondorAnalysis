library(ggplot2)
library(scales)
library(data.table)
setwd("/home/arcs/Oct14/DataCSV")
getwd()

newdata <- fread("Oct2017Efficiency.csv")
###################################################################
############# Studying the structure of Data ######################
###################################################################
names(newdata)
str(newdata)
summary(newdata)

###################################################################
############# Conversion to numeric values ########################
###################################################################
newdata[,"RemoteWallClockTime"] <- as.numeric(unlist(newdata[,"RemoteWallClockTime"])) #RemoteWallClockTime
newdata[, "ExitCode"] <- as.numeric(unlist(newdata[, "ExitCode"]))
newdata[, "MemoryUsage"] <- as.numeric(unlist(newdata[, "MemoryUsage"]))
newdata[, "MATCH_HEPSPEC"] <- as.numeric(unlist(newdata[, "MATCH_HEPSPEC"]))
newdata[, "MATCH_TotalCpus"] <- as.numeric(unlist(newdata[, "MATCH_TotalCpus"]))



###################################################################
##################### Data Cleansing ##############################
###################################################################

#newdata1 <- subset(newdata, newdata$ExitCode == 0) # Removing the failed Jobs
#newdata1[, c(ExitCode) := NULL]
newdata2 <- subset(newdata, select = c(RemoteSysCpu, RemoteUserCpu, RemoteWallClockTime, MATCH_HEPSPEC, MATCH_TotalCpus, RequestCpus))
###################################################################
############### Computation of efficiency #########################
###################################################################

newdata2$CPUTime <- newdata2$RemoteSysCpu + newdata2$RemoteUserCpu 
newdata2$WallTime <- newdata2$RemoteWallClockTime 
newdata2$HEPSPEC_TotalCpus <- newdata2$MATCH_HEPSPEC/ newdata2$MATCH_TotalCpus
newdata2$NWallTime <- newdata2$WallTime * newdata2$RequestCpus * newdata2$HEPSPEC_TotalCpus
newdata2$NCPUTime <- newdata2$CPUTime * newdata2$HEPSPEC_TotalCpus
newdata2$NEfficiency <- newdata2$NCPUTime/ newdata2$NWallTime

#Cleanseing data by removing NA rows

newdata2 <- na.omit(newdata2)


graph1 <- ggplot(subset(newdata2, newdata2$NEfficiency < 1.2), aes(x = NEfficiency)) +
  geom_histogram( color = "Black", fill = "Blue", bins = 50, alpha = 0.5 ) #+
#scale_x_continuous(bandwidth = 0.1 )
graph1 + labs(title= "CPU Time vs WallTime", x= "Normalised Efficiency", y = "Number of Jobs")

graph2 <- ggplot(newdata1, aes(x = newdata1$Efficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0)) #+
#scale_x_continuous( xlim = c(0,1.2), bandwidth = 0.1)
graph2 + labs(title= "CPU Time vs WallTime", x= "Effiency = CPU Time/WallTime", y = "log10(Number of Jobs)")




#newdata2 <- subset(newdata2, newdata2$NWallTime != 0)


TotalCPUTime <- sum(as.numeric(newdata2$NCPUTime))
TotalWallTime <- sum(newdata2$NWallTime)
TotalCPUTime
TotalWallTime
CumulativeEfficiency <- TotalCPUTime/TotalWallTime
CumulativeEfficiency
