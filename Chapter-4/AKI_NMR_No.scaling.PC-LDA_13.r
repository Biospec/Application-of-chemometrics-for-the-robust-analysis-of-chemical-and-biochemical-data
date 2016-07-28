rm(list=ls(all=T))
library(MASS)
library(caret)
B = 100
k=1
j=13
Accuracy_LDA = matrix(0, nrow=B, ncol=1)
final_table_LDA = matrix(0, nrow = 2, ncol = 2) 
for(i in 1:B){
load(sprintf("C:/Users/Piotr/Documents/AKI.data9/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:702],mean)
  std.train = sapply(train[,2:702],sd)
  train = train[,2:702]
  test =  test[,2:702]
  
      pc.train = prcomp(train,center = F,scale.=F)

      X.train = as.matrix(pc.train$x[,1:j])
      X.test = predict(pc.train, test)

  lda.model = lda(as.matrix(X.train),Class.train)
  lda.prediction = predict(lda.model, new = as.matrix(X.test[,1:j]))
  final_table_LDA = final_table_LDA + table(Class.test,lda.prediction$class)
  
  Accuracy_LDA[k,1] = 100*(sum(diag(final_table_LDA))/sum(final_table_LDA))
  k=k+1
  flush.console()
}
print(mean(Accuracy_LDA))