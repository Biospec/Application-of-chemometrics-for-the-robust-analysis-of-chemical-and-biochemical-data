rm(list=ls(all=T))
set.seed(0004)
library(cvTools)
library(caret)
library(base)
library(e1071)

################### SVM linear ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.SVM.linear = matrix(0, nrow = 4, ncol = 4)
Accuracy.SVM.linear = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.SVM.linear) = c("Iterations")
sensitivity.acetone.SVM.linear = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.SVM.linear = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.SVM.linear = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.SVM.linear = matrix(0, nrow=100, ncol=1)
specificity.acetone.SVM.linear = matrix(0, nrow=100, ncol=1)
specificity.DMMP.SVM.linear = matrix(0, nrow=100, ncol=1)
specificity.methanol.SVM.linear = matrix(0, nrow=100, ncol=1)
specificity.propanol.SVM.linear = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.SVM.linear= matrix(0, nrow = 4, ncol = 4)

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

      svm.model = svm(train, class.train, kernel = "linear")
      svm.pred = predict(svm.model, new = test, kernel = "linear")

      final.table.SVM.linear = final.table.SVM.linear + table(class.test, svm.pred)
      iterations.table.SVM.linear = iterations.table.SVM.linear + table(class.test, svm.pred)
      }

  outputFile = "final.table.SVM.linear.csv"
  write.table(final.table.SVM.linear, file = outputFile, sep = ",",j)

    Accuracy.SVM.linear[k,1] = sum(diag(final.table.SVM.linear))/sum(final.table.SVM.linear)
    sensitivity.acetone.SVM.linear[k,1] = final.table.SVM.linear[1,1]/sum(final.table.SVM.linear[,1])
    sensitivity.DMMP.SVM.linear[k,1] = final.table.SVM.linear[2,2]/sum(final.table.SVM.linear[,2])
    sensitivity.methanol.SVM.linear[k,1] = final.table.SVM.linear[3,3]/sum(final.table.SVM.linear[,3])
    sensitivity.propanol.SVM.linear[k,1] = final.table.SVM.linear[4,4]/sum(final.table.SVM.linear[,4])
    specificity.acetone.SVM.linear[k,1] = sum(final.table.SVM.linear[-1,-1])/(sum(final.table.SVM.linear)-sum(final.table.SVM.linear[,1]))
    specificity.DMMP.SVM.linear[k,1] = sum(final.table.SVM.linear[-2,-2])/(sum(final.table.SVM.linear)-sum(final.table.SVM.linear[,2]))
    specificity.methanol.SVM.linear[k,1] = sum(final.table.SVM.linear[-3,-3])/(sum(final.table.SVM.linear)-sum(final.table.SVM.linear[,3]))
    specificity.propanol.SVM.linear[k,1] = sum(final.table.SVM.linear[-4,-4])/(sum(final.table.SVM.linear)-sum(final.table.SVM.linear[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.SVM.linear)

print(sum(sensitivity.acetone.SVM.linear)/100)
print(sum(sensitivity.DMMP.SVM.linear)/100)
print(sum(sensitivity.methanol.SVM.linear)/100)
print(sum(sensitivity.propanol.SVM.linear)/100)
print(sum(specificity.acetone.SVM.linear)/100)
print(sum(specificity.DMMP.SVM.linear)/100)
print(sum(specificity.methanol.SVM.linear)/100)
print(sum(specificity.propanol.SVM.linear)/100)
print(sum(Accuracy.SVM.linear)/100)
sd(as.vector(Accuracy.SVM.linear))

outputFile = "sensitivity.acetone.SVM.linear.csv"
write.table(sensitivity.acetone.SVM.linear, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.SVM.linear.csv"
write.table(sensitivity.DMMP.SVM.linear, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.SVM.linear.csv"
write.table(sensitivity.methanol.SVM.linear, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.SVM.linear.csv"
write.table(sensitivity.propanol.SVM.linear, file = outputFile, sep = ",")

outputFile = "specificity.acetone.SVM.linear.csv"
write.table(specificity.acetone.SVM.linear, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.SVM.linear.csv"
write.table(specificity.DMMP.SVM.linear, file = outputFile, sep = ",")

outputFile = "specificity.methanol.SVM.linear.csv"
write.table(specificity.methanol.SVM.linear, file = outputFile, sep = ",")

outputFile = "specificity.propanol.SVM.linear.csv"
write.table(specificity.propanol.SVM.linear, file = outputFile, sep = ",")

outputFile = "Accuracy.SVM.linear.csv"
write.table(Accuracy.SVM.linear, file = outputFile, sep = ",")
X11()
accuracy = read.table("Accuracy.SVM.linear.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of linear SVM accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_SVM_L.jpg")
dev.off();
