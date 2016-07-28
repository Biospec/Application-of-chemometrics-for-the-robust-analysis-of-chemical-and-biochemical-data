set.seed(0002)
library(cvTools)
library(caret)
library(base)
library(MASS)
library(pls)
################### PLSDA ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.PLSDA = matrix(0, nrow = 4, ncol = 4)
Accuracy.PLSDA = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.PLSDA) = c("Iterations")
sensitivity.acetone.PLSDA = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.PLSDA = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.PLSDA = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.PLSDA = matrix(0, nrow=100, ncol=1)
specificity.acetone.PLSDA = matrix(0, nrow=100, ncol=1)
specificity.DMMP.PLSDA = matrix(0, nrow=100, ncol=1)
specificity.methanol.PLSDA = matrix(0, nrow=100, ncol=1)
specificity.propanol.PLSDA = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.PLSDA = matrix(0, nrow = 4, ncol = 4)

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

      plsda.model = plsda(train, class.train,ncomp = 6)
      plsda.pred = predict(plsda.model, new = test, ncomp = 6, type = "class")

      final.table.PLSDA = final.table.PLSDA + table(class.test, plsda.pred)
      iterations.table.PLSDA = iterations.table.PLSDA + table(class.test, plsda.pred)
      }
  outputFile = "final.table.PLSDA.csv"
  write.table(final.table.PLSDA, file = outputFile, sep = ",",j)

    Accuracy.PLSDA[k,1] = sum(diag(final.table.PLSDA))/sum(final.table.PLSDA)

    sensitivity.acetone.PLSDA[k,1] = final.table.PLSDA[1,1]/sum(final.table.PLSDA[,1])
    sensitivity.DMMP.PLSDA[k,1] = final.table.PLSDA[2,2]/sum(final.table.PLSDA[,2])
    sensitivity.methanol.PLSDA[k,1] = final.table.PLSDA[3,3]/sum(final.table.PLSDA[,3])
    sensitivity.propanol.PLSDA[k,1] = final.table.PLSDA[4,4]/sum(final.table.PLSDA[,4])
    specificity.acetone.PLSDA[k,1] = sum(final.table.PLSDA[-1,-1])/(sum(final.table.PLSDA)-sum(final.table.PLSDA[,1]))
    specificity.DMMP.PLSDA[k,1] = sum(final.table.PLSDA[-2,-2])/(sum(final.table.PLSDA)-sum(final.table.PLSDA[,2]))
    specificity.methanol.PLSDA[k,1] = sum(final.table.PLSDA[-3,-3])/(sum(final.table.PLSDA)-sum(final.table.PLSDA[,3]))
    specificity.propanol.PLSDA[k,1] = sum(final.table.PLSDA[-4,-4])/(sum(final.table.PLSDA)-sum(final.table.PLSDA[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.PLSDA)

print(sum(sensitivity.acetone.PLSDA)/100)
print(sum(sensitivity.DMMP.PLSDA)/100)
print(sum(sensitivity.methanol.PLSDA)/100)
print(sum(sensitivity.propanol.PLSDA)/100)
print(sum(specificity.acetone.PLSDA)/100)
print(sum(specificity.DMMP.PLSDA)/100)
print(sum(specificity.methanol.PLSDA)/100)
print(sum(specificity.propanol.PLSDA)/100)
print(sum(Accuracy.PLSDA)/100)
sd(as.vector(Accuracy.PLSDA))

outputFile = "sensitivity.acetone.PLSDA.csv"
write.table(sensitivity.acetone.PLSDA, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.PLSDA.csv"
write.table(sensitivity.DMMP.PLSDA, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.PLSDA.csv"
write.table(sensitivity.methanol.PLSDA, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.PLSDA.csv"
write.table(sensitivity.propanol.PLSDA, file = outputFile, sep = ",")

outputFile = "specificity.acetone.PLSDA.csv"
write.table(specificity.acetone.PLSDA, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.PLSDA.csv"
write.table(specificity.DMMP.PLSDA, file = outputFile, sep = ",")

outputFile = "specificity.methanol.PLSDA.csv"
write.table(specificity.methanol.PLSDA, file = outputFile, sep = ",")

outputFile = "specificity.propanol.PLSDA.csv"
write.table(specificity.propanol.PLSDA, file = outputFile, sep = ",")

outputFile = "Accuracy.PLSDA.csv"
write.table(Accuracy.PLSDA, file = outputFile, sep = ",")
X11()
accuracy = read.table("Accuracy.PLSDA.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of PLSDA accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_PLSDA.jpg")
dev.off();