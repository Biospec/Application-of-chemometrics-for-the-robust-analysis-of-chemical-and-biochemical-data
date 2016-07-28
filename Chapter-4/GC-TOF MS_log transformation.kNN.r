rm(list=ls(all=T))
library(class)
B = 100
k=1
Accuracy_kNN = matrix(0, nrow=B, ncol=1)
final_table = matrix(0, nrow = 3, ncol = 3) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Justyna/Documents/Kusano/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  train = log(train[,2:172])
  test = log(test[,2:172])
  test[test<(-1.2040)]=-1.5
  train[train<(-1.2040)]=-1.5
test.knn = knn(train,test, cl=Class.train, k = 1, prob=T)
final_table = final_table + table(Class.test,test.knn)
Accuracy_kNN[k,1] = 100*(sum(diag(final_table))/sum(final_table))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "GC-MS.NO.scaling_kNN.csv"
write.table(Accuracy_kNN, file = outputFile, sep = ",")