rm(list=ls(all=T))
library(randomForest)
B = 100
k=1
Accuracy_RF = matrix(0,nrow=B,ncol=1)
final_table = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/NMR.diabetes/data%d.RData",i))
  Class.train = train[,1]
  Class.test = test[,1]
  train = train[,2:189]
  test =  test[,2:189]

  rf.model = randomForest(train,Class.train,importance=T, proximity=T)
  rf.prediction = predict(rf.model, new = test)
  final_table = final_table + table(Class.test,rf.prediction)
  Accuracy_RF[k,1] = 100*(sum(diag(final_table))/sum(final_table))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "NMR.diabetes.NO.scaling_RF.csv"
write.table(Accuracy_RF, file = outputFile, sep = ",")