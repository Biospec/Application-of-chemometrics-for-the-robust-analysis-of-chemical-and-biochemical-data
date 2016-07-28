rm(list=ls(all=TRUE))
library(MASS)
B = 100
Accuracy_LDA = matrix(0, nrow = B, ncol = 1)
k = 1
j=10
final_table_LDA = matrix(0, nrow = 3, ncol = 3)
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/Kusano/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  train = train[,2:172]
  test =  test[,2:172]
      pc.train = prcomp(train,scale.=F)

      X.train = as.matrix(pc.train$x[,1:j])
      X.test = predict(pc.train, test)

  lda.model = lda(as.matrix(X.train),Class.train)
  lda.prediction = predict(lda.model, new = as.matrix(X.test[,1:j]))
  final_table_LDA = final_table_LDA + table(Class.test,lda.prediction$class)
  Accuracy_LDA[k,1] = 100*(sum(diag(final_table_LDA))/sum(final_table_LDA))
  k=k+1
  print(i)
  flush.console()
}
outputFile = "GC-MS.NO.scaling.PC_LDA_10.csv"
write.table(Accuracy_LDA, file = outputFile, sep = ",")