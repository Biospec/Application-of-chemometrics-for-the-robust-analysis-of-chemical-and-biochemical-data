library(cvTools)
library(caret)
library(base)
library(MASS)
library(pls)
library(e1071)

data = read.table("dw_data.csv", sep = ",", header=T)
Accuracy = matrix(0, nrow=10, ncol=1)
sensitivity.acetone = matrix(0, nrow=10, ncol=1)
sensitivity.DMMP = matrix(0, nrow=10, ncol=1)
sensitivity.methanol = matrix(0, nrow=10, ncol=1)
sensitivity.propanol = matrix(0, nrow=10, ncol=1)
specificity.acetone = matrix(0, nrow=10, ncol=1)
specificity.DMMP = matrix(0, nrow=10, ncol=1)
specificity.methanol = matrix(0, nrow=10, ncol=1)
specificity.propanol = matrix(0, nrow=10, ncol=1)
k=1
for (j in 1:10){
folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
final.table = matrix(0, nrow = 4, ncol = 4)

ptm = proc.time()
for (i in 1:10){
train = data[folds$subsets[folds$which != i], ]
test = data[folds$subsets[folds$which == i], ]
class.train = train[,1]
class.test = test[,1]
mean.train = mean(train[,2:13])
std.train = sd(train[,2:13])
train = scale(train[,2:13])
test = test[,2:13]
  for (p in 1:12){
    test[,p] = (test[,p] - mean.train[p])/std.train[p]}
    lda.model = lda(train, class.train)
    lda.pred = predict(lda.model, new = test)
final.table = final.table + table(class.test, lda.pred$class)
}
print(confusionMatrix(final.table))
#outputFile = "final.table.csv"
#write.table(final.table, file = outputFile, sep = ",",j)
Accuracy[k,1] = sum(diag(final.table))/sum(final.table)
sensitivity.acetone[k,1] = final.table[1,1]/sum(final.table[,1])
sensitivity.DMMP[k,1] = final.table[2,2]/sum(final.table[,2])
sensitivity.methanol[k,1] = final.table[3,3]/sum(final.table[,3])
sensitivity.propanol[k,1] = final.table[4,4]/sum(final.table[,4])
specificity.acetone[k,1] = sum(final.table[-1,-1])/(sum(final.table)-sum(final.table[,1]))
specificity.DMMP[k,1] = sum(final.table[-2,-2])/(sum(final.table)-sum(final.table[,2]))
specificity.methanol[k,1] = sum(final.table[-3,-3])/(sum(final.table)-sum(final.table[,3]))
specificity.propanol[k,1] = sum(final.table[-4,-4])/(sum(final.table)-sum(final.table[,4]))
k=k+1
flush.console()
}
print(sum(sensitivity.acetone)/10)
print(sum(sensitivity.DMMP)/10)
print(sum(sensitivity.methanol)/10)
print(sum(sensitivity.propanol)/10)
print(sum(specificity.acetone)/10)
print(sum(specificity.DMMP)/10)
print(sum(specificity.methanol)/10)
print(sum(specificity.propanol)/10)
print(sum(Accuracy)/10)

outputFile = "final.table.csv"
write.table(final.table, file = outputFile, sep = ",")

outputFile = "Accuracy.csv"
write.table(Accuracy, file = outputFile, sep = ",")
proc.time() - ptm
confusionMatrix(final.table)
