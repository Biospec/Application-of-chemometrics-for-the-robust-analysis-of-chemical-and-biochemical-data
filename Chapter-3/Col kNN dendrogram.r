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
fileName = "C:/Users/user/Documents/Proba.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Proba.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")

rawData = rawData[c(1:30,91:120,181:210),]

Class = rNames[c(1:30,91:120,181:210),1]
groupCodes = c(rep("Normoxia", 30), rep("Hypoxia",30), rep("Anoxia",30))
rownames(rawData) = groupCodes
colorCodes <- c(Normoxia="black", Hypoxia="red", Anoxia="green")

X_dist=dist(scale(rawData))
X_clust = hclust(X_dist,method="ward")

dend = as.dendrogram(X_clust)

# Assigning the labels of dendrogram object with new colors:
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
plot(dend,ylab="Distance",main="(D) kNN")