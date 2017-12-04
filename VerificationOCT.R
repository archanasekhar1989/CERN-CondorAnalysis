library(data.table)
setwd("/home/arcs/Oct14/DataCSV")
getwd()

data_web <- fread("OctVerification.csv")
data_condor <- fread("Oct2017Efficiency_VO.csv")

############### Function to print values #########################
printf <- function(...) cat(sprintf(...))

###################################################################
############# Studying the structure of Data ######################
###################################################################
names(data_web)
str(data_web)
summary(data_web)
unique(data_web$Resource)
data_web$`Number of Jobs` <- as.numeric(unlist(data_web[, data_web$`Number of Jobs`]))
data_web <- na.omit(data_web)
printf("\nTotal no of Condor jobs from website: %s", sum(data_web$`Number of Jobs`))
data_lsf <- subset(data_web, Resource == "lsf")
printf("\nNo of lsf jobs from website: %s", sum(data_lsf$`Number of Jobs`))
data_cloud <- subset(data_web, Resource == "cloud")
printf("\nNo of lsf jobs from website: %s", sum(data_cloud$`Number of Jobs`))
data_web <- subset(data_web, Resource == "condor")
printf("\nNo of Condor jobs from website: %s", sum(data_web$`Number of Jobs`))
unique(data_web$Month)
unique(data_web$VO)
#alice_web <- subset(data_web, VO == "alice")


names(data_condor)
str(data_condor)
summary(data_condor)
unique(data_condor$x509UserProxyVOName)
#data_condor <- subset(data_condor, ExitCode == "0")
#alice_hdfs <- subset(data_condor, data_condor$x509UserProxyVOName == "alice")
unique(data_condor$x509UserProxyVOName)
unique(data_hdfs$ExitCode)

###################################################################
############# Conversion to numeric values ########################
###################################################################
data_web$NCPU <- as.numeric(unlist(data_web[, "Normalised CPU Duration (hs06d)"]))
data_web$NWall <- as.numeric(unlist(data_web[, "Normalised Wall Duration (hs06d)"]))
data_web$`Number of Jobs` <- as.numeric(unlist(data_web[, data_web$`Number of Jobs`]))

TotalCPU_web <- sum(data_web$NCPU)
TotalWall_web <- sum(data_web$NWall)

Efficiency_web <- TotalCPU_web/TotalWall_web



data_hdfs[,"RemoteWallClockTime"] <- as.numeric(unlist(data_hdfs[,"RemoteWallClockTime"])) #RemoteWallClockTime
data_hdfs[, "ExitCode"] <- as.numeric(unlist(data_hdfs[, "ExitCode"]))
data_hdfs[, "MATCH_HEPSPEC"] <- as.numeric(unlist(data_hdfs[, "MATCH_HEPSPEC"]))
data_hdfs[, "MATCH_TotalCpus"] <- as.numeric(unlist(data_hdfs[, "MATCH_TotalCpus"]))


###################################################################
##################### Data Cleansing ##############################
###################################################################


str(data_hdfs)
data_hdfs <- na.omit(data_hdfs)

printf("\nNo of Condor jobs from website: %s", sum(data_web$`Number of Jobs`))
printf("\nNo of Condor jobs from HDFS: %s", nrow(data_condor))

data_hdfs$CPUTime <- data_hdfs$RemoteSysCpu + data_hdfs$RemoteUserCpu 
data_hdfs$WallTime <- data_hdfs$RemoteWallClockTime #- data_hdfs2$CumulativeSuspensionTime
str(data_hdfs)
data_hdfs$HEPSPEC_TotalCpus <- data_hdfs$MATCH_HEPSPEC/ data_hdfs$MATCH_TotalCpus
data_hdfs$NWallTime <- data_hdfs$WallTime * data_hdfs$RequestCpus * data_hdfs$HEPSPEC_TotalCpus
data_hdfs$NCPUTime <- data_hdfs$CPUTime * data_hdfs$HEPSPEC_TotalCpus
TotalWallTime_hdfs <- sum(data_hdfs$NWallTime)
TotalCPUTime_hdfs <- sum(data_hdfs$NCPUTime)
Efficiency_hdfs <- TotalCPUTime_hdfs/TotalWallTime_hdfs
