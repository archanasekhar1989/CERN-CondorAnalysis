# To extract 

import json
import csv
import pickle

from pyspark import HiveContext, SparkConf, SparkContext
from pyspark.sql import SQLContext

conf = SparkConf()
conf.setMaster("local")
conf.setAppName("JSON Read")
conf.set("spark.executor.memory", "1g")
sc = SparkContext(conf = conf)
hiveContext = HiveContext(sc)
sqlContext = SQLContext(sc)

def toCSVLine(data):
        return ','.join(str(d) for d in data)


#############################################################################################
##########Read the JSON data into data frame and write it in CSV#############################
#############################################################################################


df1 = sqlContext.read.json("hdfs:///user/accservice/batch/condor/2017/11/*") #Read it into dataframe.  Filename: "hdfs:///user/accservice/batch/condor/2017/11/*".
#df1.printSchema() #To Print the schema of database
df1.registerTempTable("df1") #Register the dataframe
df2 = sqlContext.sql("select ExitBySignal,  ExitReason, ExitSignal, ExitStatus,MemoryUsage, RequestMemory, default_maxMemory, maxMemory, JobUniverse,  RequestCpus, MATCH_HEPSPEC, MATCH_TotalCpus, RemoteWallClockTime, ExitCode, RemoteSysCpu, RemoteUserCpu, x509UserProxyVOName from df1") #Extract the values
#df2.show(500) # Print the first 500 value on Screen


######################### Write the values into file in csv format#############################

lines = df2.map(toCSVLine)
lines.saveAsTextFile("hdfs:///user/aarun/Nov2017Efficiency_final.csv")


######################### Parquet File######################################
df2.write.parquet("Nov2017Efficiency.parquet")
