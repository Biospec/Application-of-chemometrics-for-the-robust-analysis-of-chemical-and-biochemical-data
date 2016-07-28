rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)


#################################################
# INPUT DATA
#################################################
# CLEAN2 COMBINED DATA
#################################################
fileName = "C:/Users/user/Documents/Proba.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Proba.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################
# load this bit if you want to replace all NaN with 0

mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = mydata[c(1:30,91:120,181:210),]
mydata = scale(mydata, center = T, scale = T)
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random")
pc = pca(mydata, ncomp = 18, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N","H","A")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X[,1],X[,2],type="p",col=co, pch=pch1, xlab="PC1 31.83%", ylab="PC2 12.55%",main="PCA for data without drug with NaN replace with 0")
legend("topleft", c("N","H","A"), cex = 1, pch = 1:3, col = 1:3)
biplot(pc,xlab="PC1", ylab="PC2",main="PCA biplot for data without drug with NaN replace with 0")
plot3d(X[,1],X[,2],X[,3],type="s",col=co,xlab="PC1", ylab="PC2",zlab = "PC3")

fit = lda(Y ~ X)
fScores = X %*% fit$scaling

plot(fScores[,c(1,2)], type = "p", xlab = "LD1", pch = pch1, col = co, ylab = "LD2",cex = 1.5)
legend("bottomleft", c("Normoxia","Hypoxia","Anoxia"), cex = 1, pch = 1:3, col = 1:3)