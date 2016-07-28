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
# load this bit if you want to replace all NaN with median
#load this bit if you want to replace all NaN with mean for each class
N = replace(rawData[1:30,],is.na(rawData[1:30,]),mean(rawData[1:30,],na.rm = T))
ND1 = replace(rawData[31:60,],is.na(rawData[31:60,]),mean(rawData[31:60,],na.rm = T))
ND2 = replace(rawData[61:90,],is.na(rawData[61:90,]),mean(rawData[61:90,],na.rm = T))
H = replace(rawData[91:120,],is.na(rawData[91:120,]),mean(rawData[91:120,],na.rm = T))
HD1 = replace(rawData[121:150,],is.na(rawData[121:150,]),mean(rawData[121:150,],na.rm = T))
HD2 = replace(rawData[151:180,],is.na(rawData[151:180,]),mean(rawData[151:180,],na.rm = T))
A = replace(rawData[181:210,],is.na(rawData[181:210,]),mean(rawData[181:210,],na.rm = T))
AD1 = replace(rawData[211:240,],is.na(rawData[211:240,]),mean(rawData[211:240,],na.rm = T))
AD2 = replace(rawData[241:270,],is.na(rawData[241:270,]),mean(rawData[241:270,],na.rm = T))
rawData = rbind(N, ND1, ND2, H, HD1, HD2, A, AD1, AD2)
mydata = rawData
mydata = rawData
mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = mydata[c(1:30,91:120,181:210),]
mydata = scale(mydata, center = T, scale = T)
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random")
pc = pca(mydata, ncomp = 37, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N","H","A")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X[,1],X[,2],type="p",col=co, pch=pch1, xlab="PC1 14.98%", ylab="PC2 10.34%",main="PCA for data without drug with NaN replace with mean")
legend("topleft", c("N","H","A"), cex = 1, pch = 1:3, col = 1:3)
biplot(pc,xlab="PC1 14.98%", ylab="PC2 10.34%",main="PCA biplot for data without drug with NaN replace with mean")
plot3d(X[,1],X[,2],X[,3],type="s",col=co,xlab="PC1 14.98%", ylab="PC2 10.34%",zlab = "PC3 6.69%")

fit = lda(Y ~ X)
fScores = X %*% fit$scaling

plot(fScores[,c(1,2)], type = "p", xlab = "LD1", pch = pch1, col = co, ylab = "LD2",cex = 1.5)
legend("bottomright", c("Normoxia","Hypoxia","Anoxia"), cex = 1, pch = 1:3, col = 1:3)