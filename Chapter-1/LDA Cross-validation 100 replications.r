rm(list=ls(all=T))
set.seed(0001)
library(cvTools)
library(caret)
library(base)
library(MASS)

################### LDA ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
iterations.table.LDA = matrix(0, nrow = 4, ncol = 4)
Accuracy.LDA = matrix(0, nrow=100, ncol=1)
colnames(Accuracy.LDA) = c("Iterations")
sensitivity.acetone.LDA = matrix(0, nrow=100, ncol=1)
sensitivity.DMMP.LDA = matrix(0, nrow=100, ncol=1)
sensitivity.methanol.LDA = matrix(0, nrow=100, ncol=1)
sensitivity.propanol.LDA = matrix(0, nrow=100, ncol=1)
specificity.acetone.LDA = matrix(0, nrow=100, ncol=1)
specificity.DMMP.LDA = matrix(0, nrow=100, ncol=1)
specificity.methanol.LDA = matrix(0, nrow=100, ncol=1)
specificity.propanol.LDA = matrix(0, nrow=100, ncol=1)
k=1  # acts as a counter
ptm = proc.time()
for (j in 1:100){
  folds <- cvFolds(nrow(data), K = 10, type="random",R=1)
  final.table.LDA = matrix(0, nrow = 4, ncol = 4)
    
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
      
      final.table.LDA = final.table.LDA + table(class.test, lda.pred$class)
      iterations.table.LDA = iterations.table.LDA + table(class.test, lda.pred$class)
      }
  
  outputFile = "final.table.LDA.csv"
  write.table(final.table.LDA, file = outputFile, sep = ",",j)

    Accuracy.LDA[k,1] = sum(diag(final.table.LDA))/sum(final.table.LDA)

    sensitivity.acetone.LDA[k,1] = final.table.LDA[1,1]/sum(final.table.LDA[,1])
    sensitivity.DMMP.LDA[k,1] = final.table.LDA[2,2]/sum(final.table.LDA[,2])
    sensitivity.methanol.LDA[k,1] = final.table.LDA[3,3]/sum(final.table.LDA[,3])
    sensitivity.propanol.LDA[k,1] = final.table.LDA[4,4]/sum(final.table.LDA[,4])
    specificity.acetone.LDA[k,1] = sum(final.table.LDA[-1,-1])/(sum(final.table.LDA)-sum(final.table.LDA[,1]))
    specificity.DMMP.LDA[k,1] = sum(final.table.LDA[-2,-2])/(sum(final.table.LDA)-sum(final.table.LDA[,2]))
    specificity.methanol.LDA[k,1] = sum(final.table.LDA[-3,-3])/(sum(final.table.LDA)-sum(final.table.LDA[,3]))
    specificity.propanol.LDA[k,1] = sum(final.table.LDA[-4,-4])/(sum(final.table.LDA)-sum(final.table.LDA[,4]))
    k=k+1 # to fills in bove lines in the next row with each iterations
}
proc.time() - ptm
confusionMatrix(iterations.table.LDA)
 
print(sum(sensitivity.acetone.LDA)/100)
print(sum(sensitivity.DMMP.LDA)/100)
print(sum(sensitivity.methanol.LDA)/100)
print(sum(sensitivity.propanol.LDA)/100)
print(sum(specificity.acetone.LDA)/100)
print(sum(specificity.DMMP.LDA)/100)
print(sum(specificity.methanol.LDA)/100)
print(sum(specificity.propanol.LDA)/100)
print(sum(Accuracy.LDA)/100)
sd(as.vector(Accuracy.LDA))

outputFile = "sensitivity.acetone.LDA.csv"
write.table(sensitivity.acetone.LDA, file = outputFile, sep = ",")

outputFile = "sensitivity.DMMP.LDA.csv"
write.table(sensitivity.DMMP.LDA, file = outputFile, sep = ",")

outputFile = "sensitivity.methanol.LDA.csv"
write.table(sensitivity.methanol.LDA, file = outputFile, sep = ",")

outputFile = "sensitivity.propanol.LDA.csv"
write.table(sensitivity.propanol.LDA, file = outputFile, sep = ",")

outputFile = "specificity.acetone.LDA.csv"
write.table(specificity.acetone.LDA, file = outputFile, sep = ",")

outputFile = "specificity.DMMP.LDA.csv"
write.table(specificity.DMMP.LDA, file = outputFile, sep = ",")

outputFile = "specificity.methanol.LDA.csv"
write.table(specificity.methanol.LDA, file = outputFile, sep = ",")

outputFile = "specificity.propanol.LDA.csv"
write.table(specificity.propanol.LDA, file = outputFile, sep = ",")

outputFile = "Accuracy.LDA.csv"
write.table(Accuracy.LDA, file = outputFile, sep = ",")

X11()
accuracy = read.table("Accuracy.LDA.csv", sep=",", header = T)
accuracy = accuracy$Iterations
hist(accuracy, breaks = 10, main = "Histogram of LDA accuracy",xlab = "Accuracy",prob=T)
lines(density(accuracy, adjust = 2), lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/Justyna/Documents/Hist_CV_LDA.jpg")
dev.off();
