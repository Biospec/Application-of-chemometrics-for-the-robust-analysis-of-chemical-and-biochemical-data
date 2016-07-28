set.seed(0003)
library(cvTools)
library(caret)
library(base)
################### Random Forests ##########################
library(randomForest)

iterations.table.RF = matrix(0, nrow = 4, ncol = 4)
data = read.table("dw_data.csv", sep = ",", header=T)
Accuracy.RF = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.RF) = c("Iterations")
sensitivity.acetone.RF = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.RF = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.RF = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.RF = matrix(0, nrow=100, ncol=1)
specificity.acetone.RF = matrix(0, nrow=100, ncol=1)
specificity.DMMP.RF = matrix(0, nrow=100, ncol=1)
specificity.methanol.RF = matrix(0, nrow=100, ncol=1)
specificity.propanol.RF = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.RF = matrix(0, nrow = 4, ncol = 4)

    for (i in 1:10){
      train = data[folds$subsets[folds$which != i], ]
      test = data[folds$subsets[folds$which == i], ]

      class.train = train[,1]
      class.test = test[,1]
      train = train[,2:13]
      test = test[,2:13]

      rf.model = randomForest(train, class.train, proximity=T, importance=T)
      rf.pred = predict(rf.model, new = test)

      final.table.RF = final.table.RF + table(class.test, rf.pred)
      iterations.table.RF = iterations.table.RF + table(class.test, rf.pred)
      }

    outputFile = "final.table.RF.csv"
    write.table(final.table.RF, file = outputFile, sep = ",",j)

    Accuracy.RF[k,1] = sum(diag(final.table.RF))/sum(final.table.RF)

    sensitivity.acetone.RF[k,1] = final.table.RF[1,1]/sum(final.table.RF[,1])
    sensitivity.DMMP.RF[k,1] = final.table.RF[2,2]/sum(final.table.RF[,2])
    sensitivity.methanol.RF[k,1] = final.table.RF[3,3]/sum(final.table.RF[,3])
    sensitivity.propanol.RF[k,1] = final.table.RF[4,4]/sum(final.table.RF[,4])
    specificity.acetone.RF[k,1] = sum(final.table.RF[-1,-1])/(sum(final.table.RF)-sum(final.table.RF[,1]))
    specificity.DMMP.RF[k,1] = sum(final.table.RF[-2,-2])/(sum(final.table.RF)-sum(final.table.RF[,2]))
    specificity.methanol.RF[k,1] = sum(final.table.RF[-3,-3])/(sum(final.table.RF)-sum(final.table.RF[,3]))
    specificity.propanol.RF[k,1] = sum(final.table.RF[-4,-4])/(sum(final.table.RF)-sum(final.table.RF[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.RF)

print(sum(sensitivity.acetone.RF)/100)
print(sum(sensitivity.DMMP.RF)/100)
print(sum(sensitivity.methanol.RF)/100)
print(sum(sensitivity.propanol.RF)/100)
print(sum(specificity.acetone.RF)/100)
print(sum(specificity.DMMP.RF)/100)
print(sum(specificity.methanol.RF)/100)
print(sum(specificity.propanol.RF)/100)
print(sum(Accuracy.RF)/100)
sd(as.vector(Accuracy.RF))

outputFile = "sensitivity.acetone.RF.csv"
write.table(sensitivity.acetone.RF, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.RF.csv"
write.table(sensitivity.DMMP.RF, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.RF.csv"
write.table(sensitivity.methanol.RF, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.RF.csv"
write.table(sensitivity.propanol.RF, file = outputFile, sep = ",")

outputFile = "specificity.acetone.RF.csv"
write.table(specificity.acetone.RF, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.RF.csv"
write.table(specificity.DMMP.RF, file = outputFile, sep = ",")

outputFile = "specificity.methanol.RF.csv"
write.table(specificity.methanol.RF, file = outputFile, sep = ",")

outputFile = "specificity.propanol.RF.csv"
write.table(specificity.propanol.RF, file = outputFile, sep = ",")

outputFile = "Accuracy.RF.csv"
write.table(Accuracy.RF, file = outputFile, sep = ",")

X11()
accuracy = read.table("Accuracy.RF.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of Random Forests accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_RF.jpg")
dev.off();
