rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)
library(mixOmics)
library(scatterplot3d)

#############  ZERO ############################


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
# load this bit if you want to replace all NaN with 0
is.nan.data.frame = function(x)
do.call(cbind, lapply(x, is.nan))
rawData[is.nan(rawData)] = 0
mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames


mydata = scale(mydata, center = T, scale = T)

pc = pca(mydata, ncomp = 21, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3
Y[which(Y == u[4])] = 4
Y[which(Y == u[5])] = 5
Y[which(Y == u[6])] = 6
Y[which(Y == u[7])] = 7
Y[which(Y == u[8])] = 8
Y[which(Y == u[9])] = 9

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X, xlab="PC1",ylab="PC2",type="n")
points(X,col=co, pch=pch1,cex = 1.5 )
legend("topright", c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2"), cex = 1, pch = 1:9, col = 1:9)

############# MEAN ####################################

rm(list=ls(all=TRUE))
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

mydata = scale(mydata, center = T, scale = T)
pc = pca(mydata, ncomp = 37, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3
Y[which(Y == u[4])] = 4
Y[which(Y == u[5])] = 5
Y[which(Y == u[6])] = 6
Y[which(Y == u[7])] = 7
Y[which(Y == u[8])] = 8
Y[which(Y == u[9])] = 9

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X,xlab="PC1", ylab="PC2",type="n")
points(X,col=co, pch=pch1,cex = 1.5 )
legend("bottomright", c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2"), cex = 1, pch = 1:9, col = 1:9)


############### MEDIAN ###############################


rm(list=ls(all=TRUE))

fileName = "C:/Users/user/Documents/Hypoxia.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Hypoxia.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################
# load this bit if you want to replace all NaN with median
#load this bit if you want to replace all NaN with median for each class
N = replace(rawData[1:30,],is.na(rawData[1:30,]),median(rawData[1:30,],na.rm = T))
ND1 = replace(rawData[31:60,],is.na(rawData[31:60,]),median(rawData[31:60,],na.rm = T))
ND2 = replace(rawData[61:90,],is.na(rawData[61:90,]),median(rawData[61:90,],na.rm = T))
H = replace(rawData[91:120,],is.na(rawData[91:120,]),median(rawData[91:120,],na.rm = T))
HD1 = replace(rawData[121:150,],is.na(rawData[121:150,]),median(rawData[121:150,],na.rm = T))
HD2 = replace(rawData[151:180,],is.na(rawData[151:180,]),median(rawData[151:180,],na.rm = T))
A = replace(rawData[181:210,],is.na(rawData[181:210,]),median(rawData[181:210,],na.rm = T))
AD1 = replace(rawData[211:240,],is.na(rawData[211:240,]),median(rawData[211:240,],na.rm = T))
AD2 = replace(rawData[241:270,],is.na(rawData[241:270,]),median(rawData[241:270,],na.rm = T))
rawData = rbind(N, ND1, ND2, H, HD1, HD2, A, AD1, AD2)
mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = scale(mydata, center = T, scale = T)
pc = pca(mydata, ncomp = 27, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3
Y[which(Y == u[4])] = 4
Y[which(Y == u[5])] = 5
Y[which(Y == u[6])] = 6
Y[which(Y == u[7])] = 7
Y[which(Y == u[8])] = 8
Y[which(Y == u[9])] = 9

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X,xlab="PC1", ylab="PC2",type="n")
points(X,col=co, pch=pch1,cex = 1.5)
legend("bottomleft", c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2"), cex = 1, pch = 1:9, col = 1:9)

###################### kNN #####################################

rm(list=ls(all=TRUE))

fileName = "C:/Users/user/Documents/Proba.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Proba.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################

mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = scale(mydata, center = T, scale = T)
pc = pca(mydata, ncomp = 18, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3
Y[which(Y == u[4])] = 4
Y[which(Y == u[5])] = 5
Y[which(Y == u[6])] = 6
Y[which(Y == u[7])] = 7
Y[which(Y == u[8])] = 8
Y[which(Y == u[9])] = 9

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X,xlab="PC1", ylab="PC2",type="n")
points(X,col=co, pch=pch1,cex = 1.5)
legend("topright", c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2"), cex = 1, pch = 1:9, col = 1:9)

###################### RF #####################################

rm(list=ls(all=TRUE))

fileName = "C:/Users/user/Documents/Proba.RF.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Proba.RF.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################

mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = scale(mydata, center = T, scale = T)
pc = pca(mydata, ncomp = 14, scale=F, center = F)

X = as.matrix(pc$x)

u = c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2")
Y = rownames(mydata)

Y[which(Y == u[1])] = 1
Y[which(Y == u[2])] = 2
Y[which(Y == u[3])] = 3
Y[which(Y == u[4])] = 4
Y[which(Y == u[5])] = 5
Y[which(Y == u[6])] = 6
Y[which(Y == u[7])] = 7
Y[which(Y == u[8])] = 8
Y[which(Y == u[9])] = 9

Y = as.numeric(Y)

pch1 = Y
co = Y

plot(X,xlab="PC1", ylab="PC2",type="n")
points(X,col=co, pch=pch1,cex = 1.5)
legend("bottomleft", c("N", "ND1", "ND2","H", "HD1", "HD2","A", "AD1", "AD2"), cex = 1, pch = 1:9, col = 1:9)
