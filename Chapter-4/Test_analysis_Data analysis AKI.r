rm(list=ls(all=TRUE))
B = 10
pc = seq(1,50,1)


res = matrix(0, nrow = length(pc), ncol = 2)
colnames(res) = c("Nr PCs","accuracy")
k = 1
for (j in 1:length(pc))
{
final_table_LDA = matrix(0, nrow = 2, ncol = 2)
for(i in 1:B){
load(sprintf("C:/Users/Piotr/Documents/AKI/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:702],mean)
  std.train = sapply(train[,2:702],sd)
  train = train[,2:702]

  test =  test[,2:702]



      pc.train = prcomp(train,scale. = FALSE)

      X.train = as.matrix(pc.train$x[,1:j])
      X.test = predict(pc.train, test)

  lda.model = lda(as.matrix(X.train),Class.train)
  lda.prediction = predict(lda.model, new = as.matrix(X.test[,1:j]))
  final_table_LDA = final_table_LDA + table(Class.test,lda.prediction$class)
  }
  res[k,1] = pc[j]
  res[k,2] = 100*(sum(diag(final_table_LDA))/sum(final_table_LDA))
  k=k+1
  print(j)
  flush.console()

}