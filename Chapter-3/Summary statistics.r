rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)
library(devtools)
# loading the package
library(dendextend)
library(psych)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/user/Documents/Hypoxia.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Hypoxia.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################
mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames
Normoxia = rawData[1:30,]
Hypoxia = rawData[91:120,]
Anoxia = rawData[181:210,]
normoxia.table=describe(Normoxia,na.rm=T,skew=T)
hypoxia.table=describe(Hypoxia,na.rm=T,skew=T)
anoxia.table=describe(Anoxia,na.rm=T,skew=T)


#### Normoxia
outputFile = "normoxia.table.csv"
write.table(normoxia.table, file = outputFile, sep = ",")

average.normoxia = (normoxia.table$n/30*100-100)*-1
mean(average.normoxia)
average.normoxia=as.matrix(average.normoxia)

outputFile = "average.normoxia.csv"
write.table(average.normoxia, file = outputFile, sep = ",")

#### Hypoxia
outputFile = "hypoxia.table.csv"
write.table(hypoxia.table, file = outputFile, sep = ",")

average.hypoxia = (hypoxia.table$n/30*100-100)*-1
mean(average.hypoxia)
average.hypoxia=as.matrix(average.hypoxia)

outputFile = "average.hypoxia.csv"
write.table(average.hypoxia, file = outputFile, sep = ",")

#### Anoxia
outputFile = "anoxia.table.csv"
write.table(anoxia.table, file = outputFile, sep = ",")

average.anoxia = (anoxia.table$n/30*100-100)*-1
mean(average.anoxia)
average.anoxia=as.matrix(average.anoxia)

outputFile = "average.anoxia.csv"
write.table(average.anoxia, file = outputFile, sep = ",")

