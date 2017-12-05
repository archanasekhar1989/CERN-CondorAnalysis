############## Aim of this program is to check if number of jobs in HDFS and S3 ########################
##############          system match for the month of november                  ########################
##############                  as it did'nt match for Oct                      ########################

library(data.table)
setwd("/home/arcs/Oct14/DataCSV")
getwd()

data_web <- fread("NovWeb.csv")
data_hdfs <- fread(input = "Nov2017Efficiency_VO.csv", sep = ",", fill = TRUE)

############### Function to print values #########################
printf <- function(...) cat(sprintf(...))

###################################################################
############# Studying the structure of Data ######################
###################################################################
names(data_web)
str(data_web)
summary(data_web)
unique(data_web$Resource) # Tocheck the types of resources


###################################################################
############# Conversion to numeric values ########################
###################################################################
data_web$`Number of Jobs` <- as.numeric(unlist(data_web[, data_web$`Number of Jobs`]))
summary(data_web)

###################################################################
############# Removing jobs with NA in      #######################
#############     Particular Col           ########################
###################################################################
data_web <- data_web[!is.na(data_web$`Number of Jobs`), ]


###################################################################
############# Comparing jobs from HDFS and Web data ###############
###################################################################

############ To check for the particular Month ####################

printf("\n Month of evaluation: %s", unique(data_web$Month))

printf("\nTotal no of jobs from website: %s", sum(data_web$`Number of Jobs`))

data_lsf <- subset(data_web, Resource == "lsf")
printf("\nNo of lsf jobs from website: %s", sum(data_lsf$`Number of Jobs`))

data_cloud <- subset(data_web, Resource == "cloud")
printf("\nNo of cloud jobs from website: %s", sum(data_cloud$`Number of Jobs`))

data_condor <- subset(data_web, Resource == "condor")
web_condor_jobs = sum(data_web$`Number of Jobs`)
printf("\nNo of Condor jobs from website: %s", sum(data_condor$`Number of Jobs`))
unique(data_condor$Infrastructure)
web_condor_grid <- subset(data_condor, data_condor$Infrastructure == "grid")
printf("\nNo of Condor:grid jobs from website: %s", sum(web_condor_grid$`Number of Jobs`))

web_condor_local <- subset(data_condor, data_condor$Infrastructure == "local")
printf("\nNo of Condor:grid jobs from website: %s", sum(web_condor_local$`Number of Jobs`))

hdfs_condor_jobs = nrow(data_hdfs)
printf("\nTotal no of jobs from HDFS: %d", nrow(data_hdfs))
diff = web_condor_jobs - hdfs_condor_jobs
printf("\nNo of missing jobs in HDFS System: %d", diff)

################### To check if all VOs are captured ######################
unique(data_web$VO)
unique(data_hdfs$x509UserProxyVOName)
 

VO = unique(data_hdfs$x509UserProxyVOName)

for (vo in VO){
  printf("\n\n\n************ VO Name: %s ***************\n", vo)
  sub_Data <- subset(data_hdfs, x509UserProxyVOName == vo)
  printf("\nNumber of observation from HDFS: %d", nrow(sub_Data))
  sub_Data_web <- subset(data_condor, data_condor$VO == vo)
  printf("\nNumber of observation from Website: %d", sum(sub_Data_web$`Number of Jobs`))
}
