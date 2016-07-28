rm(list=ls(all=T))
library(randomForest)
B = 100
k=1
Accuracy_RF = matrix(0,nrow=B,ncol=1)
final_table = matrix(0, nrow = 3, ncol = 3)
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/Kusano/data%d.RData",i))
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:172],mean)
  std.train = sapply(train[,2:172],sd)
  train = train[,2:172]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ std.train[v]
    }
  test =  test[,2:172]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]
      }
rf.model = randomForest(train,Class.train,importance=T, proximity=T)
rf.prediction = predict(rf.model, new = test)
final_table = final_table + table(Class.test,rf.prediction)
Accuracy_RF[k,1] = 100*(sum(diag(final_table))/sum(final_table))
k=k+1
print(i)
flush.console()
}
outputFile = "GC-MS.Auto.scaling_RF.csv"
write.table(Accuracy_RF, file = outputFile, sep = ",")