rm(list=ls(all=TRUE))
B = 100
Accuracy_LDA = matrix(0, nrow = B, ncol = 1)
k = 1
j=19
final_table_LDA = matrix(0, nrow = 2, ncol = 2)
for(i in 1:B){
load(sprintf("C:/Users/Piotr/Documents/AKI.data/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:702],mean)
  std.train = sapply(train[,2:702],sd)

 train = train[,2:702]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ std.train[v]
    }
  test =  test[,2:702]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]
      }

      pc.train = prcomp(train)

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
outputFile = "AKI.NMR.Auto.scaling.PC_LDA_19.csv"
write.table(Accuracy_LDA, file = outputFile, sep = ",")