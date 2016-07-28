rm(list=ls(all=T))
library(class)
B = 100
k=1
Accuracy_kNN = matrix(0,nrow=B,ncol=1)
final_table = matrix(0, nrow = 2, ncol = 2) 
  for(i in 1:B){
  load(sprintf("C:/Users/Justyna/Documents/AKI.data/data%d.RData",i))
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:702],mean)
  std.train = sapply(train[,2:702],sd)
  train = train[,2:702]
  test =  test[,2:702]
  test.knn = knn(train,test, cl=Class.train, k = 27, prob=T)
  final_table = final_table + table(Class.test,test.knn)
  Accuracy_kNN[k,1] = 100*(sum(diag(final_table))/sum(final_table))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "AKI.NMR.NO.scaling_kNN.csv"
write.table(Accuracy_kNN, file = outputFile, sep = ",")