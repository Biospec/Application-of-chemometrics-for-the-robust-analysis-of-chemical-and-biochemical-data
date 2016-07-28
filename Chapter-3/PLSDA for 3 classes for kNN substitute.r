rm(list=ls(all=T))
library(MASS)
library(caret)
library(pls)

Accuracy_PLSDA = matrix(0, nrow=100, ncol=1)
final_table_PLSDA = matrix(0, nrow = 3, ncol = 3) # Table for confusion matrix
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
P = 24
for(i in 1:B){
load(sprintf("C:/Users/user/Documents/3classes kNN bis/data%d.RData",i))
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:53],mean)
  std.train = sapply(train[,2:53],sd)
  train = train[,2:53]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/std.train[v]
    }
  test =  test[,2:53]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]
      }
  plsda.model = plsda(as.matrix(train),Class.train,ncomp = P)
  plsda.prediction = predict(plsda.model, new = as.matrix(test),ncomp = P, type = "class")
  final_table_PLSDA = final_table_PLSDA + table(Class.test,plsda.prediction)
  Accuracy_PLSDA[k,1] = sum(diag(final_table_PLSDA))/sum(final_table_PLSDA)
  k=k+1
  flush.console()
}
print(mean(Accuracy_PLSDA))
