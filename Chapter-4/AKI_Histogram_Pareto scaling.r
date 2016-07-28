rm(list=ls(all=TRUE))

library(MASS)
library(rgl)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/Piotr/Documents/AKI.data.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("AKI data.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 701)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 701)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 701)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################
mydata=rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames
n = 9 # number of compoents
  mean.mydata = apply(mydata,2,mean)
  std.mydata = apply(mydata,2,sd)

mydata = (mydata-mean.mydata)/ sqrt(std.mydata)

pc = prcomp(mydata,scale.=F)

X = as.matrix(pc$x)

u = c("Normal", "Injury")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2

Y = as.numeric(Y)

fit = lda(Y ~ X[,1:n])
LDA = X[,1:n]%*%fit$scaling

a = LDA[1:72]
b = LDA[73:106]
hist(a,col="red",main = "", xlab="LD",breaks=10,density=10,xlim=c(-6,6), ylim=c(0,25))
hist(b,add=T,col="blue",density=10, angle=-45,breaks=10)
legend(-6,25,c("Normal", "Injury"),col = par(c("red","blue")),title="(D) Pareto scaling",density=15,angle=c(45,-45),border=c("red","blue"))



