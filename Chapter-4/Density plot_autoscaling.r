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
mydata = scale(mydata, center = T, scale = T)

nComps = 36
pc = prcomp(mydata, nPcs = nComps, method = "svd")

X = as.matrix(pc$x)

u = c("type2", "Control")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2

Y = as.numeric(Y)

fit = lda(Y ~ X[,1:n])
fScores = X[,1:n] %*% fit$scaling

density = as.vector(fScores)
sm.density.compare(density, Y,lwd=2)

density.f = factor(Y, levels = c(1,2),labels = c("T2DM", "Control"))

sm.density.compare(density, Y, xlab="LD")
title(main="PC-LDA for autoscalig appraoch")

colfill<-c(2:(2+length(levels(density.f))))
legend(locator(1), levels(density.f), fill=colfill)



