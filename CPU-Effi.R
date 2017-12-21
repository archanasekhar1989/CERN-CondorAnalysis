#Aim: To study the relationship between Requested CPU, Efficieny and Normalised Efficiency

library(data.table)
library(ggplot2)


setwd("/home/arcs/Oct14/DataCSV")
getwd()
jobs <- fread(input = "Nov2017Efficiency_VO_withBigBird.csv", sep = ",", fill = TRUE)

############### Function to print values #########################
printf <- function(...) cat(sprintf(...))

###################################################################
############# Studying the structure of Data ######################
###################################################################
##names(jobs)
##str(jobs)
##summary(jobs)
printf("\nTotal number of jobs: %d\n", nrow(jobs))


###################################################################
############# Conversion to numeric values ########################
###################################################################
jobs[,"RemoteWallClockTime"] <- as.numeric(unlist(jobs[,"RemoteWallClockTime"])) #RemoteWallClockTime
jobs[, "MATCH_HEPSPEC"] <- as.numeric(unlist(jobs[, "MATCH_HEPSPEC"]))
jobs[, "MATCH_TotalCpus"] <- as.numeric(unlist(jobs[, "MATCH_TotalCpus"]))

###################################################################
############# Removing jobs with NA in      #######################
#############     Particular Col           ########################
###################################################################
jobs <- jobs[!is.na((jobs$RemoteWallClockTime))]
jobs <- jobs[!is.na(jobs$MATCH_HEPSPEC)]
jobs <- jobs[!is.na(jobs$MATCH_TotalCpus)]

#######################################################################
############# setting default values for     ##########################
#############   MATCH_HEPSPEC and MATCH_TotalCpus  ####################
#######################################################################

index <- jobs$MATCH_HEPSPEC == 0
jobs$MATCH_HEPSPEC[index] <- 80
jobs$MATCH_TotalCpus <- 8

sum(jobs$MATCH_TotalCpus == 0)

###################################################################
############### Computation of efficiency #########################
###################################################################
jobs$CPUTime <- jobs$RemoteSysCpu + jobs$RemoteUserCpu 
jobs$WallTime <- jobs$RemoteWallClockTime
jobs <- subset(jobs, jobs$WallTime != 0) #Removing jobs with WallTime = 0
jobs <- subset(jobs, jobs$MATCH_TotalCpus != 0)
jobs$HEPSPEC_TotalCpus <- jobs$MATCH_HEPSPEC/ jobs$MATCH_TotalCpus
jobs$NWallTime <- jobs$WallTime * jobs$RequestCpus * jobs$HEPSPEC_TotalCpus
jobs$NCPUTime <- jobs$CPUTime * jobs$HEPSPEC_TotalCpus
jobs <- subset(jobs, NWallTime != 0)
printf("\nTotal no of jobs after removing jobs with normalized walltime = 0: %d\n", nrow(jobs))
jobs$NEfficiency <- jobs$NCPUTime/jobs$NWallTime
Total_NEfficiency <- sum(jobs$NCPUTime)/sum(jobs$NWallTime)

printf("\n\n Normalised Efficiency(For all jobs):")
print(Total_NEfficiency)

#######################################################################
high_effi_jobs <- subset(jobs, jobs$NEfficiency > 1.2)
low_effi_jobs <- subset(jobs, jobs$NEfficiency <= 1.2)
nrow(subset(high_effi_jobs, high_effi_jobs$RequestCpus > 1))
high_RequestCpus <- subset(jobs, jobs$RequestCpus > 1)


cor(jobs$RequestCpus, jobs$NEfficiency)
cor(jobs$NEfficiency, jobs$HEPSPEC_TotalCpus)
cor(jobs$WallTime, jobs$NEfficiency)
cor(jobs$CPUTime, jobs$NEfficiency)
cor(jobs)

graph1 <- ggplot(high_RequestCpus, aes(x = NEfficiency)) +
  geom_histogram( color = "Black", fill = "Blue", bins = 50, alpha = 0.5 ) 
graph1 + labs(title= "Normalized Efficiency of Jobs with Request-CPU > 1", x= "Normalized Efficiency", y = "Number of Jobs")

graph2 <- ggplot(jobs, aes(x = HEPSPEC_TotalCpus)) +
  geom_histogram( color = "Black", fill = "Blue", bins = 50, alpha = 0.5 ) 
graph2 + labs(title= "Effect of HEPSPEC per CPU(Quality of Machine)", x= "HEPSPEC_TotalCpus(HEPSPEC per CPU)", y = "Number of Jobs")

jobs_Effi_less_0.2 <- subset(low_effi_jobs, low_effi_jobs$NEfficiency <= 0.2)
jobs_Effi_grt_0.8 <- subset(low_effi_jobs, low_effi_jobs$NEfficiency > 0.8)


graph3 <- ggplot(jobs_Effi_less_0.2, aes(x = NEfficiency)) +
  geom_histogram(color = "Black", fill = "Blue", bins = 50, alpha = 0.5)
graph3 + labs(title= "Normalized Efficiency of Jobs with efficiency less than 0.2", x= "Normalized Efficiency", y = "Number of Jobs")

notuseful_jobs <- subset(jobs_Effi_less_0.2, jobs_Effi_less_0.2$CPUTime == 0)
nrow(subset(notuseful_jobs, RemoteWallClockTime == 0))
graph4 <- ggplot(notuseful_jobs, aes(x = notuseful_jobs$RemoteWallClockTime)) +
  geom_histogram(color = "Black", fill = "Blue", bins = 50, alpha = 0.5) +
  scale_y_continuous(trans="log10", expand=c(0,0)) 
graph4 + labs(title= "Wall Time of Jobs with CPU Time = 0", x= "Wall Time", y = "Number of Jobs")

unique(jobs$RequestCpus)
