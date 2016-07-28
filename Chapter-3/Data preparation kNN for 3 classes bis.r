rm(list=ls(all=TRUE))

library(MASS)
library(rgl)
library(chemometrics)
library(impute)

rm(list=ls(all=TRUE))

t.hypoxia = read.table("Test.hypoxia.3.classes.csv",sep=",",header=T)
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
write.table(hypoxia, file = "Proba.3.classes.csv",row.names=F, col.names = F,sep=",")

mydata =  read.table("Proba.3.classes.csv",sep=",",header=T)

n=length(mydata[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train = mydata[index,]
test = mydata[-index,]
save(train,test,file = sprintf("C:/Users/user/Documents/3classes kNN bis/data%d.RData",i))}