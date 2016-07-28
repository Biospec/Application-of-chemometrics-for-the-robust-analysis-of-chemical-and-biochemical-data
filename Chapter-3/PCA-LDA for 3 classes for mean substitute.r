rm(list=ls(all=T))
library(MASS)
library(caret)

Accuracy_LDA = matrix(0, nrow=100, ncol=1)
final_table_LDA = matrix(0, nrow = 3, ncol = 3) # Table for confusion matrix
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
j=13
for(i in 1:B){
load(sprintf("C:/Users/user/Documents/3classes mean/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:53],mean)
  std.train = sapply(train[,2:53],sd)
min.train = sapply(train[,2:53],min)
 max.train = sapply(train[,2:53],max)
 train = train[,2:53]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/std.train[v]
    }
  test =  test[,2:53]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]
      }
   
      pc.train = princomp(train)

      X.train = as.matrix(pc.train$scores[,1:j])
      X.test = predict(pc.train, test)
      
  lda.model = lda(as.matrix(X.train),Class.train)                                  
  lda.prediction = predict(lda.model, new = as.matrix(X.test[,1:j]))
  final_table_LDA = final_table_LDA + table(Class.test,lda.prediction$class)
  Accuracy_LDA[k,1] = sum(diag(final_table_LDA))/sum(final_table_LDA)
  k=k+1
  flush.console()
}
print(mean(Accuracy_LDA))
