rm(list=ls(all=T))
library(e1071)
B = 100
k=1
Accuracy_SVM = matrix(0, nrow=B, ncol=1)
final_table = matrix(0, nrow = 2, ncol = 2)
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/NMR.diabetes/data%d.RData",i))
  train = unique(train)
  test = unique(test)
  Class.train = train[,1]
  Class.test = test[,1]
  train = train[,2:189]
  test = test[,2:189]

  svm.model = svm(train,Class.train,kernel = "linear",scale=F)
  svm.prediction = predict(svm.model, new = test,kernel = "linear")
  final_table = final_table + table(Class.test,svm.prediction)
  Accuracy_SVM[k,1] = 100*(sum(diag(final_table))/sum(final_table))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "NMR.diabetes.NO.scaling_SVM.csv"
write.table(Accuracy_SVM, file = outputFile, sep = ",")