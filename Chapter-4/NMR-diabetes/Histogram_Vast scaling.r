rm(list=ls(all=TRUE))

library(MASS)
library(rgl)


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
mydata=rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames
n = 36 # number of compoents
  mean.mydata = apply(mydata,2,mean)
  std.mydata = apply(mydata,2,sd)

mydata = (mydata-mean.mydata)/ std.mydata*(mean.mydata/std.mydata)

pc = prcomp(mydata, scale.=F)

X = as.matrix(pc$x)

u = c("type2", "Control")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2

Y = as.numeric(Y)

fit = lda(Y ~ X[,1:n])
LDA = X[,1:n]%*%fit$scaling

a = LDA[1:48]
b = LDA[49:132]
hist(a,col="red",main = "", xlab="LD",breaks=10,density=10,xlim=c(-7,6), ylim=c(0,25))
hist(b,add=T,col="blue",density=10, angle=-45,breaks=10)
legend(-6,25,c("T2DM","Control"),col = par(c("red","blue")),title="(E) Vast scaling",density=15,angle=c(45,-45),border=c("red","blue"))





