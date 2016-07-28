rm(list=ls(all=TRUE))

library(MASS)
library(rgl)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/Piotr/Documents/Kusano.test.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Kusano.test.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 171)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 171)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 171)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################
mydata=rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames
n = 14 # number of components
  mean.mydata = apply(mydata,2,mean)
  std.mydata = apply(mydata,2,sd)

mydata = (mydata-mean.mydata)/ sqrt(std.mydata)

pc = prcomp(mydata, scale.=F)

X = as.matrix(pc$x)

u = c("Wild", "tt4", "mto 1")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3

Y = as.numeric(Y)

fit = lda(Y ~ X[,1:n])
fScores = X[,1:n] %*% fit$scaling

pch1 = Y
co = Y

pointSizePlot = 1.2
pointSizeLegend = 1.2

offsetx = 0.7
xmi = min(fScores[,1]) - offsetx
xma = max(fScores[,1]) + offsetx

offsety = 0.7
ymi = min(fScores[,2]) - offsety
yma = max(fScores[,2]) + offsety

par(mar = c(4.5, 4.1, 2.1, 1))

plot(fScores[,c(1,2)], type = "p", xlim = c(xmi, xma), ylim = c(ymi, yma), xlab = "LD 1", pch = pch1, col = co, ylab = "LD 2", main = sprintf("(D) PC-LDA based on Pareto scaling"),cex=1.5,lty=2,cex.lab = 1.5,cex.main=1.5)
legend("topleft", c("Wild", "tt4", "mto 1"), cex = 1.5, pch = 1:3, col = 1:3, pt.cex = pointSizeLegend)
