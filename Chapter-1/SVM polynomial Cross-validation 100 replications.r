rm(list=ls(all=T))
set.seed(0005)
library(cvTools)
library(caret)
library(base)
library(e1071)

################### SVM polynomial ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.SVM.polynomial = matrix(0, nrow = 4, ncol = 4)
Accuracy.SVM.polynomial = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.SVM.polynomial) = c("Iterations")
sensitivity.acetone.SVM.polynomial = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.SVM.polynomial = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.SVM.polynomial = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.SVM.polynomial = matrix(0, nrow=100, ncol=1)
specificity.acetone.SVM.polynomial = matrix(0, nrow=100, ncol=1)
specificity.DMMP.SVM.polynomial = matrix(0, nrow=100, ncol=1)
specificity.methanol.SVM.polynomial = matrix(0, nrow=100, ncol=1)
specificity.propanol.SVM.polynomial = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.SVM.polynomial= matrix(0, nrow = 4, ncol = 4)

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

      svm.model = svm(train, class.train, kernel = "polynomial", gamma = .18, degree = 2, coef0 = 2.5)
      svm.pred = predict(svm.model, new = test, kernel = "polynomial", gamma = .18, degree = 2, coef0 = 2.5)

      final.table.SVM.polynomial = final.table.SVM.polynomial + table(class.test, svm.pred)
      iterations.table.SVM.polynomial = iterations.table.SVM.polynomial +  table(class.test, svm.pred)
      }

  outputFile = "final.table.SVM.polynomial.csv"
  write.table(final.table.SVM.polynomial, file = outputFile, sep = ",",j)

    Accuracy.SVM.polynomial[k,1] = sum(diag(final.table.SVM.polynomial))/sum(final.table.SVM.polynomial)
    sensitivity.acetone.SVM.polynomial[k,1] = final.table.SVM.polynomial[1,1]/sum(final.table.SVM.polynomial[,1])
    sensitivity.DMMP.SVM.polynomial[k,1] = final.table.SVM.polynomial[2,2]/sum(final.table.SVM.polynomial[,2])
    sensitivity.methanol.SVM.polynomial[k,1] = final.table.SVM.polynomial[3,3]/sum(final.table.SVM.polynomial[,3])
    sensitivity.propanol.SVM.polynomial[k,1] = final.table.SVM.polynomial[4,4]/sum(final.table.SVM.polynomial[,4])
    specificity.acetone.SVM.polynomial[k,1] = sum(final.table.SVM.polynomial[-1,-1])/(sum(final.table.SVM.polynomial)-sum(final.table.SVM.polynomial[,1]))
    specificity.DMMP.SVM.polynomial[k,1] = sum(final.table.SVM.polynomial[-2,-2])/(sum(final.table.SVM.polynomial)-sum(final.table.SVM.polynomial[,2]))
    specificity.methanol.SVM.polynomial[k,1] = sum(final.table.SVM.polynomial[-3,-3])/(sum(final.table.SVM.polynomial)-sum(final.table.SVM.polynomial[,3]))
    specificity.propanol.SVM.polynomial[k,1] = sum(final.table.SVM.polynomial[-4,-4])/(sum(final.table.SVM.polynomial)-sum(final.table.SVM.polynomial[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.SVM.polynomial)

print(sum(sensitivity.acetone.SVM.polynomial)/100)
print(sum(sensitivity.DMMP.SVM.polynomial)/100)
print(sum(sensitivity.methanol.SVM.polynomial)/100)
print(sum(sensitivity.propanol.SVM.polynomial)/100)
print(sum(specificity.acetone.SVM.polynomial)/100)
print(sum(specificity.DMMP.SVM.polynomial)/100)
print(sum(specificity.methanol.SVM.polynomial)/100)
print(sum(specificity.propanol.SVM.polynomial)/100)
print(sum(Accuracy.SVM.polynomial)/100)
sd(as.vector(Accuracy.SVM.polynomial))

outputFile = "sensitivity.acetone.SVM.polynomial.csv"
write.table(sensitivity.acetone.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.SVM.polynomial.csv"
write.table(sensitivity.DMMP.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.SVM.polynomial.csv"
write.table(sensitivity.methanol.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.SVM.polynomial.csv"
write.table(sensitivity.propanol.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "specificity.acetone.SVM.polynomial.csv"
write.table(specificity.acetone.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.SVM.polynomial.csv"
write.table(specificity.DMMP.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "specificity.methanol.SVM.polynomial.csv"
write.table(specificity.methanol.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "specificity.propanol.SVM.polynomial.csv"
write.table(specificity.propanol.SVM.polynomial, file = outputFile, sep = ",")

outputFile = "Accuracy.SVM.polynomial.csv"
write.table(Accuracy.SVM.polynomial, file = outputFile, sep = ",")
X11()
accuracy = read.table("Accuracy.SVM.polynomial.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of polynomial SVM accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_SVM_P.jpg")
dev.off();
