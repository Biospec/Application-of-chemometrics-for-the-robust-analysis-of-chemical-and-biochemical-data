rm(list=ls(all=T))
library(randomForest)
B = 100
k=1
Accuracy_RF = matrix(0,nrow=B,ncol=1)
final_table = matrix(0, nrow = 3, ncol = 3) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Justyna/Documents/Kusano/data%d.RData",i))
  Class.train = train[,1]
  Class.test = test[,1]
  train = log(train[,2:172])
  test = log(test[,2:172])
  test[test<(-1.2040)]=-1.5
  train[train<(-1.2040)]=-1.5
rf.model = randomForest(train,Class.train,importance=T, proximity=T,ntree=350,mtry=11)
rf.prediction = predict(rf.model, new = test)
final_table = final_table + table(Class.test,rf.prediction)
Accuracy_RF[k,1] = 100*(sum(diag(final_table))/sum(final_table))
k=k+1
print(i)
flush.console()
}
outputFile = "GC-MS.NO.scaling_RF.csv"
write.table(Accuracy_RF, file = outputFile, sep = ",")