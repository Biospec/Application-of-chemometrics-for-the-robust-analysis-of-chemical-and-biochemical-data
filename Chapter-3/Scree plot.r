rm(list=ls(all=TRUE))
library(MASS)
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


####################                    MEAN            ########################


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
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random",plot.opt = FALSE)
boxplot(res$ExplVar, xlab = "Number of components",ylab = "Explained variance", ylim = 0:1,border="red",cex.lab = 1.5)


###################                    MEDIAN            #######################



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
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random",plot.opt = FALSE)
boxplot(add=T,res$ExplVar, border="green")



######################                    0            #########################


rm(list=ls(all=TRUE))
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
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random",plot.opt = FALSE)
boxplot(add=T,res$ExplVar, border="black")

##########################  kNN ############################

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


mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames

mydata = scale(mydata, center = T, scale = T)
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random",plot.opt = FALSE)
boxplot(add=T,res$ExplVar, border="blue")


######################## randomforest  ################################

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
res = pcaCV(mydata, segments = 10, repl = 100, segment.type = "random",plot.opt = FALSE)
boxplot(add=T,res$ExplVar, border="violetred")

grid(NA,20,lty = "dashed",equilogs = TRUE)
legend("bottomright", c("Zero","Mean","Median","kNN","RF"), cex = 1, pch = 0, col = c("black","red","green","blue","violetred"))