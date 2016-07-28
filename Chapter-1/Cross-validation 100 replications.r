set.seed(1982)
library(cvTools)
library(caret)
library(base)
library(MASS)

################### LDA ##########################

data = read.table("dw_data.csv", sep = ",", header=T)
Accuracy.LDA = matrix(0, nrow=100, ncol=1)
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
print(j) 
flush.console()
}
proc.time() - ptm
confusionMatrix(final.table.LDA)
 
print(sum(sensitivity.acetone.LDA)/100)
print(sum(sensitivity.DMMP.LDA)/100)
print(sum(sensitivity.methanol.LDA)/100)
print(sum(sensitivity.propanol.LDA)/100)
print(sum(specificity.acetone.LDA)/100)
print(sum(specificity.DMMP.LDA)/100)
print(sum(specificity.methanol.LDA)/100)
print(sum(specificity.propanol.LDA)/100)
print(sum(Accuracy.LDA)/100)

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



################### Random Forests ##########################

library(randomForest)

Accuracy.RF = matrix(0, nrow=100, ncol=1)
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
print(j)
flush.console()
}

proc.time() - ptm
confusionMatrix(final.table.RF)

print(sum(sensitivity.acetone.RF)/100)
print(sum(sensitivity.DMMP.RF)/100)
print(sum(sensitivity.methanol.RF)/100)
print(sum(sensitivity.propanol.RF)/100)
print(sum(specificity.acetone.RF)/100)
print(sum(specificity.DMMP.RF)/100)
print(sum(specificity.methanol.RF)/100)
print(sum(specificity.propanol.RF)/100)
print(sum(Accuracy.RF)/100)

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

