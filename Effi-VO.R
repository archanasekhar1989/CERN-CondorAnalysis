###################################################################
###### Objective of this program is to ############################
###### study normalised efficiency of jobs and ####################
###### To study normalised efficiency for each VO #################
###################################################################

library(data.table)
library(ggplot2)

setwd("/home/arcs/Oct14/DataCSV")
getwd()
jobs <<- fread("Oct2017Efficiency_VO.csv")

############### Function to print values #########################
printf <- function(...) cat(sprintf(...))


###################################################################
############# Studying the structure of Data ######################
###################################################################
names(jobs)
str(jobs)
summary(jobs)
printf("\nTotal number of jobs: %d\n", nrow(jobs))

###################################################################
############# Conversion to numeric values ########################
###################################################################
if(jobs$RemoteWallClockTime != "0"){
  if(jobs$MATCH_HEPSPEC == "0"){
    jobs$MATCH_HEPSPEC <- "80"
    jobs$MATCH_TotalCpus <- "8"   

  }
}


jobs[,"RemoteWallClockTime"] <- as.numeric(unlist(jobs[,"RemoteWallClockTime"])) #RemoteWallClockTime
jobs[, "MATCH_HEPSPEC"] <- as.numeric(unlist(jobs[, "MATCH_HEPSPEC"]))
jobs[, "MATCH_TotalCpus"] <- as.numeric(unlist(jobs[, "MATCH_TotalCpus"]))



###################################################################
##################### Data Cleansing ##############################
###################################################################
jobs$WallTime <- jobs$RemoteWallClockTime
jobs$HEPSPEC_TotalCpus <- jobs$MATCH_HEPSPEC/ jobs$MATCH_TotalCpus
nrow(subset(jobs, jobs$RemoteWallClockTime !=0 && jobs$MATCH_HEPSPEC == 0))
jobs <- na.omit(jobs)
printf("\nTotal no of jobs after removing NA:%d \n", nrow(jobs))



###################################################################
############### Computation of efficiency #########################
###################################################################
jobs$CPUTime <- jobs$RemoteSysCpu + jobs$RemoteUserCpu 


jobs$NWallTime <- jobs$WallTime * jobs$RequestCpus * jobs$HEPSPEC_TotalCpus
jobs$NCPUTime <- jobs$CPUTime * jobs$HEPSPEC_TotalCpus
jobs <- subset(jobs, NWallTime != 0)
printf("\nTotal no of jobs after removing jobs with normalized walltime = 0: %d\n", nrow(jobs))
jobs$NEfficiency <- jobs$NCPUTime/jobs$NWallTime
Total_NEfficiency <- sum(jobs$NCPUTime)/sum(jobs$NWallTime)

printf("\n\n Normalised Efficiency(For all jobs):")
print(Total_NEfficiency)


graph1 <- ggplot(jobs, aes(x = NEfficiency)) +
  geom_histogram( color = "Black", fill = "Blue", bins = 50, alpha = 0.5 ) 
graph1 + labs(title= "Normalized Efficiency", x= "Normalized Efficiency", y = "Number of Jobs")


graph2 <- ggplot(jobs, aes(x = NEfficiency)) +
  geom_histogram(color = "Black", fill = "cornflowerblue", bins = 50 ) +
  scale_y_continuous(trans="log10", expand=c(0,0))
graph2 + labs(title= "Normalized Efficiency (Log Scale)", x= "Normalized Efficiency", y = "Number of Jobs")

printf("\nTotal no of jobs with normalized efficiency <= 1.2: %d\n", nrow(subset(jobs, NEfficiency <= 1.2)))
printf("\nTotal no of jobs with normalized efficiency > 1.2: %d\n", nrow(subset(jobs, NEfficiency > 1.2)))


graph3 <- ggplot(subset(jobs, jobs$NEfficiency <= 1.2), aes(x = NEfficiency)) +
  geom_histogram( color = "Black", fill = "Blue", bins = 50, alpha = 0.5 )
graph3 + labs(title= "Normalized Efficiency, Jobs with Normalized Efficiency <= 1.2", x= "Normalised Efficiency", y = "Number of Jobs")


VO = unique(jobs$x509UserProxyVOName)

for (vo in VO){
  printf("\n\n\n************ VO Name: %s ***************\n", vo)
    sub_Data <- subset(jobs, x509UserProxyVOName == vo)
  printf("\nNumber of observation: %d", nrow(sub_Data))
  printf("\nPercentage of data: %f", (nrow(sub_Data)/nrow(jobs))*100)
  NEfficiency_sub <- sum(sub_Data$NCPUTime)/sum(sub_Data$NWallTime)
  printf("\nNormalized Efficiency: ")
  print(NEfficiency_sub)
}

nrow(subset(jobs, jobs$WallTime != 0 && jobs$MATCH_HEPSPEC == 0))



