rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/Piotr/Documents/AKI.test.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("AKI.test.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 701)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 701)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 701)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################

mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames


mydata = scale(mydata, center = T, scale = T)

pc = prcomp(mydata)

X = as.matrix(pc$x)

u = c("kidney normal","Kidney injury")
Y = rownames(mydata)


Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2


Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X,xlab="PC1 35.82%", ylab="PC2 11.02%",col=co, pch=Y, type="n",cex = 1,cex.lab = 1)
points(X, cex = 1.5, col = Y, pch = Y)
legend("topright", c("kidney normal","Kidney injury"), cex = 1, pch = 1:2, col = 1:2)