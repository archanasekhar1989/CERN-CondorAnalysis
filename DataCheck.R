library(data.table)
setwd("/home/arcs/Oct14/DataCSV")
getwd()

jobs_old <- fread("Nov2017Efficiency_VO.csv", sep = ",", fill = TRUE)
jobs_new <- fread(input = "Nov2017Efficiency_VO_withBigBird.csv", sep = ",", fill = TRUE)
data_web <- fread("NovWeb.csv")

############### Function to print values #########################
printf <- function(...) cat(sprintf(...))

###################################################################
############# Conversion to numeric values ########################
###################################################################
data_web$`Number of Jobs` <- as.numeric(unlist(data_web[, data_web$`Number of Jobs`]))
##summary(data_web)

###################################################################
############# Removing jobs with NA in      #######################
#############     Particular Col           ########################
###################################################################
data_web <- data_web[!is.na(data_web$`Number of Jobs`), ]


data_condor <- subset(data_web, Resource == "condor")
web_condor_jobs = sum(data_condor$`Number of Jobs`)
printf("\nNo of Condor jobs from website: %s", sum(data_condor$`Number of Jobs`))

