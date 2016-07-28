rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)


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
is.nan.data.frame = function(x)
do.call(cbind, lapply(x, is.nan))
rawData[is.nan(rawData)] = 0
rawData = rawData[c(1:30,91:120,181:210),]
Class = rNames[c(1:30,91:120,181:210),1]
mydata = data.frame(Class,rawData)


n=length(mydata[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train = mydata[index,]
test = mydata[-index,]
save(train,test,file = sprintf("C:/Users/user/Documents/3classes zero/data%d.RData",i))}