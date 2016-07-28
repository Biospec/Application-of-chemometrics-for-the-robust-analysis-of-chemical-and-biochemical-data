rm(list=ls(all=T))
set.seed(0007)
library(cvTools)
library(caret)
library(base)
library(e1071)

################### SVM sigmoid ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.SVM.sigmoid = matrix(0, nrow = 4, ncol = 4)
Accuracy.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.SVM.sigmoid) = c("Iterations")
sensitivity.acetone.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
specificity.acetone.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
specificity.DMMP.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
specificity.methanol.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
specificity.propanol.SVM.sigmoid = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.SVM.sigmoid= matrix(0, nrow = 4, ncol = 4)

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

      svm.model = svm(train, class.train, kernel = "sigmoid", gamma = .05, coef0 = 0)
      svm.pred = predict(svm.model, new = test, kernel = "sigmoid", gamma = .05, coef0 = 0)

      final.table.SVM.sigmoid = final.table.SVM.sigmoid + table(class.test, svm.pred)
      iterations.table.SVM.sigmoid = iterations.table.SVM.sigmoid + table(class.test, svm.pred)
      }

  outputFile = "final.table.SVM.sigmoid.csv"
  write.table(final.table.SVM.sigmoid, file = outputFile, sep = ",",j)

    Accuracy.SVM.sigmoid[k,1] = sum(diag(final.table.SVM.sigmoid))/sum(final.table.SVM.sigmoid)
    sensitivity.acetone.SVM.sigmoid[k,1] = final.table.SVM.sigmoid[1,1]/sum(final.table.SVM.sigmoid[,1])
    sensitivity.DMMP.SVM.sigmoid[k,1] = final.table.SVM.sigmoid[2,2]/sum(final.table.SVM.sigmoid[,2])
    sensitivity.methanol.SVM.sigmoid[k,1] = final.table.SVM.sigmoid[3,3]/sum(final.table.SVM.sigmoid[,3])
    sensitivity.propanol.SVM.sigmoid[k,1] = final.table.SVM.sigmoid[4,4]/sum(final.table.SVM.sigmoid[,4])
    specificity.acetone.SVM.sigmoid[k,1] = sum(final.table.SVM.sigmoid[-1,-1])/(sum(final.table.SVM.sigmoid)-sum(final.table.SVM.sigmoid[,1]))
    specificity.DMMP.SVM.sigmoid[k,1] = sum(final.table.SVM.sigmoid[-2,-2])/(sum(final.table.SVM.sigmoid)-sum(final.table.SVM.sigmoid[,2]))
    specificity.methanol.SVM.sigmoid[k,1] = sum(final.table.SVM.sigmoid[-3,-3])/(sum(final.table.SVM.sigmoid)-sum(final.table.SVM.sigmoid[,3]))
    specificity.propanol.SVM.sigmoid[k,1] = sum(final.table.SVM.sigmoid[-4,-4])/(sum(final.table.SVM.sigmoid)-sum(final.table.SVM.sigmoid[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.SVM.sigmoid)

print(sum(sensitivity.acetone.SVM.sigmoid)/100)
print(sum(sensitivity.DMMP.SVM.sigmoid)/100)
print(sum(sensitivity.methanol.SVM.sigmoid)/100)
print(sum(sensitivity.propanol.SVM.sigmoid)/100)
print(sum(specificity.acetone.SVM.sigmoid)/100)
print(sum(specificity.DMMP.SVM.sigmoid)/100)
print(sum(specificity.methanol.SVM.sigmoid)/100)
print(sum(specificity.propanol.SVM.sigmoid)/100)
print(sum(Accuracy.SVM.sigmoid)/100)
sd(as.vector(Accuracy.SVM.sigmoid))

outputFile = "sensitivity.acetone.SVM.sigmoid.csv"
write.table(sensitivity.acetone.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.SVM.sigmoid.csv"
write.table(sensitivity.DMMP.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.SVM.sigmoid.csv"
write.table(sensitivity.methanol.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.SVM.sigmoid.csv"
write.table(sensitivity.propanol.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "specificity.acetone.SVM.sigmoid.csv"
write.table(specificity.acetone.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.SVM.sigmoid.csv"
write.table(specificity.DMMP.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "specificity.methanol.SVM.sigmoid.csv"
write.table(specificity.methanol.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "specificity.propanol.SVM.sigmoid.csv"
write.table(specificity.propanol.SVM.sigmoid, file = outputFile, sep = ",")

outputFile = "Accuracy.SVM.sigmoid.csv"
write.table(Accuracy.SVM.sigmoid, file = outputFile, sep = ",")

X11()
accuracy = read.table("Accuracy.SVM.sigmoid.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of sigmoid SVM accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_SVM_S.jpg")
dev.off();