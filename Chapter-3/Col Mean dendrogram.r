rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)
library(devtools)
# loading the package
library(dendextend)


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
rawData = rbind(N,H,A)
Class = rNames[c(1:30,91:120,181:210),1]
groupCodes = c(rep("Normoxia", 30), rep("Hypoxia",30), rep("Anoxia",30))
rownames(rawData) = groupCodes
colorCodes <- c(Normoxia="black", Hypoxia="red", Anoxia="green")

X_dist=dist(scale(rawData))
X_clust = hclust(X_dist,method="ward")

dend = as.dendrogram(X_clust)

# Assigning the labels of dendrogram object with new colors:
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend,ylab="Distance",main = "(B) Mean")