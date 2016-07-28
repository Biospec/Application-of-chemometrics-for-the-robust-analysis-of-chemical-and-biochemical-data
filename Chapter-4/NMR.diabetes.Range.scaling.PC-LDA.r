rm(list=ls(all=T))
library(MASS)
library(caret)


B = 100
pc = seq(1,40,1)
k=1

accuracy = matrix(0, nrow = length(pc), ncol = 2)
colnames(accuracy) = c("No. PCs","accuracy")
k = 1
for (j in 1:length(pc))
{
Accuracy_LDA = matrix(0, nrow=100, ncol=1)
final_table_LDA = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
 # number of bootstrap that we want to perform on selected data set

for(i in 1:B){
load(sprintf("C:/Users/Piotr/Documents/NMR.test/data%d.RData",i))

  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = sapply(train[,2:189],mean)
  std.train = sapply(train[,2:189],sd)
  
 min.train = sapply(train[,2:189],min)
 max.train = sapply(train[,2:189],max)
 train = train[,2:189]
  for (v in 1: length(train)){
    train[,v] = (train[,v]-mean.train[v])/ (max.train[v]-min.train[v])
    }
  test =  test[,2:189]
    for (p in 1:length(test)){
      test[,p] = (test[,p]-mean.train[p])/(max.train[p]-min.train[p])
      }
      pc.train = prcomp(train,scale.=F)

      X.train = as.matrix(pc.train$x[,1:j])
      X.test = predict(pc.train, test)

  lda.model = lda(as.matrix(X.train),Class.train)
  lda.prediction = predict(lda.model, new = as.matrix(X.test[,1:j]))
  final_table_LDA = final_table_LDA + table(Class.test,lda.prediction$class)
  }
  accuracy[k,1] = pc[j]
  accuracy[k,2] = 100*(sum(diag(final_table_LDA))/sum(final_table_LDA))
  k=k+1
  print(j)
  flush.console()

}
outputFile = "NMR.diabetes.Range.scaling.PC_LDA.csv"
write.table(accuracy, file = outputFile, sep = ",")