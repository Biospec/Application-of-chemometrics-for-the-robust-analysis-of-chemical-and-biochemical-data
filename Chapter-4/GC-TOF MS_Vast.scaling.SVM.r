rm(list=ls(all=T))
library(e1071)
B = 100
k=1
Accuracy_SVM = matrix(0, nrow=B, ncol=1)
final_table = matrix(0, nrow = 3, ncol = 3) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/Kusano/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:172],mean)
  std.train = sapply(train[,2:172],sd)
  train = train[,2:172]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ std.train[v]*(mean.train[v]/std.train[v])
    }
  test =  test[,2:172]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]*(mean.train[p]/std.train[p])
      }

  svm.model = svm(train,Class.train,kernel = "linear",scale=F)
  svm.prediction = predict(svm.model, new = test,kernel = "linear")
  final_table = final_table + table(Class.test,svm.prediction)
  Accuracy_SVM[k,1] = 100*(sum(diag(final_table))/sum(final_table))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "GC-MS.Vast.scaling_SVM.csv"
write.table(Accuracy_SVM, file = outputFile, sep = ",")