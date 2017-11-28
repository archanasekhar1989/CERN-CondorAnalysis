library(data.table)
library(ggplot2)
library(scales)
library(sparklyr)
library(tidyverse)
library(leaflet)
library(rsparkling)
library(h2o)
library(DT)
library(RCurl)

x = scp("analytix.cern.ch", "/afs/cern.ch/user/a/aarun/EfficiencyData/14Oct2017EfficiencyMem.csv", "+Zxcv123", user="aarun")
Sys.setenv(SPARK_HOME = "/usr/lib/rstudio-server/bin/rserver")

setwd("/home/arcs/Oct14/")
getwd()
newdata <- read.csv("14Oct2017EfficiencyMem.csv", header = T, sep=",")

names(newdata)
str(newdata)
class(newdata)
newdata2 <- newdata
newdata[,1] <- as.numeric(as.character(newdata[,1]))


newdata$CPUTime <- newdata$RemoteSysCpu + newdata$RemoteUserCpu 
newdata$WallTime <- newdata$RemoteWallClockTime - newdata$CumulativeSuspensionTime

newdata$Efficiency <- newdata$CPUTime/ newdata$WallTime


unique(newdata$ExitCode, incomparables = FALSE)
unique(newdata$ExitSignal, incomparables = FALSE)
str(newdata)
#Cleanseing data by removing NA rows
newdata3 = na.omit(newdata$Efficiency)

newdata3

sum(is.na(newdata$RequestMemory))
sum(is.na(newdata$MemoryUsage))
sum(is.na(newdata$default_maxMemory))
sum(is.na(newdata$maxMemory))
sum(is.na(newdata$RemoteWallClockTime))
sum(is.na(newdata$ExitBySignal))
sum(is.na(newdata$ExitCode))
sum(is.na(newdata$ExitSignal))
sum(is.na(newdata$ExitStatus))
sum(is.na(newdata$RemoteSysCpu))
sum(is.na(newdata$RemoteUserCpu))
sum(is.na(newdata$CumulativeSuspensionTime))

grep("None", newdata$RequestMemory)
sum(grep("None", newdata$MemoryUsage))
grep("None", newdata$default_maxMemory)
sum(grep("None", newdata$maxMemory))

grep("None", newdata$RemoteWallClockTime)
grep("None", newdata$ExitBySignal)
grep("None", newdata$ExitSignal)
grep("None", newdata$ExitStatus)

grep("None", newdata$RemoteSysCpu)
grep("None", newdata$RemoteUserCpu)
grep("None", newdata$CumulativeSuspensionTime)
grep("None", newdata$CumulativeSuspensionTime)



E0 = subset(newdata, newdata$ExitCode == "None")
E0R0 = subset(E0, E0$RemoteWallClockTime == "None")
R0 = subset(newdata, newdata$RemoteWallClockTime == "None")
R0E0 = subset(R0, R0$ExitCode == "None")
