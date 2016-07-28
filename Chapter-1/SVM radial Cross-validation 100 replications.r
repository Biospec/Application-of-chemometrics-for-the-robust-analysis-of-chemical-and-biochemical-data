rm(list=ls(all=T))
set.seed(0006)
library(cvTools)
library(caret)
library(base)
library(e1071)

################### SVM radial ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.SVM.radial = matrix(0, nrow = 4, ncol = 4)
Accuracy.SVM.radial = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.SVM.radial) = c("Iterations")
sensitivity.acetone.SVM.radial = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.SVM.radial = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.SVM.radial = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.SVM.radial = matrix(0, nrow=100, ncol=1)
specificity.acetone.SVM.radial = matrix(0, nrow=100, ncol=1)
specificity.DMMP.SVM.radial = matrix(0, nrow=100, ncol=1)
specificity.methanol.SVM.radial = matrix(0, nrow=100, ncol=1)
specificity.propanol.SVM.radial = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
iteration.table = matrix(0, nrow = 4, ncol= 4)
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.SVM.radial= matrix(0, nrow = 4, ncol = 4)
  
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

      svm.model = svm(train, class.train, kernel = "radial", gamma = .2)
      svm.pred = predict(svm.model, new = test, kernel = "radial", gamma = .2)

      iterations.table.SVM.radial = iterations.table.SVM.radial + table(class.test, svm.pred)
      final.table.SVM.radial = final.table.SVM.radial + table(class.test, svm.pred)
      }

  outputFile = "final.table.SVM.radial.csv"
  write.table(final.table.SVM.radial, file = outputFile, sep = ",",j)

    Accuracy.SVM.radial[k,1] = sum(diag(final.table.SVM.radial))/sum(final.table.SVM.radial)
    sensitivity.acetone.SVM.radial[k,1] = final.table.SVM.radial[1,1]/sum(final.table.SVM.radial[,1])
    sensitivity.DMMP.SVM.radial[k,1] = final.table.SVM.radial[2,2]/sum(final.table.SVM.radial[,2])
    sensitivity.methanol.SVM.radial[k,1] = final.table.SVM.radial[3,3]/sum(final.table.SVM.radial[,3])
    sensitivity.propanol.SVM.radial[k,1] = final.table.SVM.radial[4,4]/sum(final.table.SVM.radial[,4])
    specificity.acetone.SVM.radial[k,1] = sum(final.table.SVM.radial[-1,-1])/(sum(final.table.SVM.radial)-sum(final.table.SVM.radial[,1]))
    specificity.DMMP.SVM.radial[k,1] = sum(final.table.SVM.radial[-2,-2])/(sum(final.table.SVM.radial)-sum(final.table.SVM.radial[,2]))
    specificity.methanol.SVM.radial[k,1] = sum(final.table.SVM.radial[-3,-3])/(sum(final.table.SVM.radial)-sum(final.table.SVM.radial[,3]))
    specificity.propanol.SVM.radial[k,1] = sum(final.table.SVM.radial[-4,-4])/(sum(final.table.SVM.radial)-sum(final.table.SVM.radial[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.SVM.radial)

print(sum(sensitivity.acetone.SVM.radial)/100)
print(sum(sensitivity.DMMP.SVM.radial)/100)
print(sum(sensitivity.methanol.SVM.radial)/100)
print(sum(sensitivity.propanol.SVM.radial)/100)
print(sum(specificity.acetone.SVM.radial)/100)
print(sum(specificity.DMMP.SVM.radial)/100)
print(sum(specificity.methanol.SVM.radial)/100)
print(sum(specificity.propanol.SVM.radial)/100)
print(sum(Accuracy.SVM.radial)/100)
sd(as.vector(Accuracy.SVM.radial))

outputFile = "sensitivity.acetone.SVM.radial.csv"
write.table(sensitivity.acetone.SVM.radial, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.SVM.radial.csv"
write.table(sensitivity.DMMP.SVM.radial, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.SVM.radial.csv"
write.table(sensitivity.methanol.SVM.radial, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.SVM.radial.csv"
write.table(sensitivity.propanol.SVM.radial, file = outputFile, sep = ",")

outputFile = "specificity.acetone.SVM.radial.csv"
write.table(specificity.acetone.SVM.radial, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.SVM.radial.csv"
write.table(specificity.DMMP.SVM.radial, file = outputFile, sep = ",")

outputFile = "specificity.methanol.SVM.radial.csv"
write.table(specificity.methanol.SVM.radial, file = outputFile, sep = ",")

outputFile = "specificity.propanol.SVM.radial.csv"
write.table(specificity.propanol.SVM.radial, file = outputFile, sep = ",")

outputFile = "Accuracy.SVM.radial.csv"
write.table(Accuracy.SVM.radial, file = outputFile, sep = ",")
X11()
accuracy = read.table("Accuracy.SVM.radial.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of radial SVM accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_SVM_R.jpg")
dev.off();