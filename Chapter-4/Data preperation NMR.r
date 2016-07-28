rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/Piotr/Documents/NMR.data.test.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("NMR.data.test.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 188)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 188)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 188)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################

Class = rNames[,1]
mydata = data.frame(Class,rawData)

n=length(mydata[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train = mydata[index,]
test = mydata[-index,]
save(train,test,file = sprintf("C:/Users/Piotr/Documents/NMR.test/data%d.RData",i))}