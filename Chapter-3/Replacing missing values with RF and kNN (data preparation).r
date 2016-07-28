##########################  kNN ############################
# Make sure that before kNN you will transpose the data to have variables 
# in the rows and observations in the columns
# here I've already done that "Test.hypoxia.csv"  
rm(list=ls(all=TRUE))

t.hypoxia = read.table("Test.hypoxia.csv",sep=",",header=T)
t.hypoxia[t.hypoxia == "NaN"]="NA"
te=t.hypoxia[-1,-1]
if(exists(".Random.seed")) rm(.Random.seed)
hypoxia.imputed = impute.knn(as.matrix(te), rowmax = 0.8)
hypoxia.knn = hypoxia.imputed$data
as.data.frame(hypoxia.knn)
hypoxia.knn = data.frame(t.hypoxia[2:53,1],hypoxia.knn)
hypoxia.knn = as.data.frame(t(hypoxia.knn))
class = t(t.hypoxia[1,])
class = as.factor(class)
hypoxia = cbind(class,hypoxia.knn)
write.table(hypoxia, file = "Proba.csv",row.names=F, col.names = F,sep=",")

######################## randomforest  ################################

rm(list=ls(all=TRUE))
fileName = "C:/Users/user/Documents/Hypoxia.csv"
#################################################

fileTitle = basename(fileName)
fileTitle = sub("Hypoxia.csv", "", fileTitle, ignore.case = TRUE)

rawData = as.matrix(read.table(fileName, skip = 1, colClasses = c(rep("NULL", 1), rep("numeric", 52)), header = FALSE, as.is = TRUE, sep = ","))
rNames = read.table(fileName, skip = 1, colClasses = c(rep("character", 1), rep("NULL", 52)), header = FALSE, as.is = TRUE, sep = ",")
cNames = read.table(fileName, nrows = 1, colClasses = c(rep("NULL", 1), rep("character", 52)), header = FALSE, as.is = TRUE, sep = ",")
################################################################################

rawData.imp <- missForest(rawData)
rawData = rawData.imp$ximp


mydata = rawData
rownames(mydata) = rNames[,1]
colnames(mydata) = cNames
write.table(mydata, file = "Proba.RF.csv",row.names=T, col.names = T,sep=",")

###### or 
rawData.imp <- missForest(rawData[c(1:30,91:120,181:210),])
rawData = rawData.imp$ximp


mydata = rawData
rownames(mydata) = rNames[c(1:30,91:120,181:210),1]
colnames(mydata) = cNames
write.table(mydata, file = "Proba.RF.test.csv",row.names=T, col.names = T,sep=",")

