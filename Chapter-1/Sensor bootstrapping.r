rm(list=ls(all=T))
#load libraries
library(MASS)
library(base)
library(pls)
library(e1071)
library(caret)
library(randomForest)

### LDA 
B = 1000
k=1
final.table.LDA = matrix(0, nrow = 4, ncol = 4)
Accuracy_LDA = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_LDA) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_LDA = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
  lda.model = lda(train,Class.train)
  lda.prediction = predict(lda.model, new = test)
  T_LDA = T_LDA + table(Class.test,lda.prediction$class)
  final.table.LDA = final.table.LDA + table(Class.test,lda.prediction$class)
  Accuracy_LDA[k,1] = sum(diag(T_LDA))/sum(T_LDA)
  k=k+1
}
print(sum(Accuracy_LDA)/1000)
sd(as.vector(Accuracy_LDA))
proc.time() - ptm
confusionMatrix(final.table.LDA)
outputFile = "Accuracy_LDA.csv"
write.table(Accuracy_LDA, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_LDA.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of LDA accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_LDA.jpg")
dev.off();


# PLSDA 
B = 1000
k=1
final.table.PLSDA = matrix(0, nrow = 4, ncol = 4)
Accuracy_PLSDA = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_PLSDA) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_PLSDA = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
      plsda.model = plsda(train,Class.train, ncomp = 8)
      plsda.prediction = predict(plsda.model, new = test, ncomp = 8, type = "class")
      T_PLSDA = T_PLSDA + table(Class.test,plsda.prediction)
      final.table.PLSDA = final.table.PLSDA + table(Class.test,plsda.prediction)
  Accuracy_PLSDA[k,1] = sum(diag(T_PLSDA))/sum(T_PLSDA)
  k=k+1
}
print(sum(Accuracy_PLSDA)/1000)
sd(as.vector(Accuracy_PLSDA))
proc.time() - ptm
confusionMatrix(final.table.PLSDA)
outputFile = "Accuracy_PLSDA.csv"
write.table(Accuracy_PLSDA, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_PLSDA.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of PLSDA accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_PLSDA.jpg")
dev.off();

# RF 
B = 1000
k=1
final.table.RF = matrix(0, nrow = 4, ncol = 4)
Accuracy_RF = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_RF) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))  
  T_RF = matrix(0, nrow = 4, ncol = 4)
  rf.model = randomForest(train[,2:13],train[,1], importance=T, proximity=T)
  rf.prediction = predict(rf.model, new = test[,2:13])
  T_RF = T_RF + table(test[,1],rf.prediction)
  final.table.RF = final.table.RF + table(test[,1],rf.prediction)
  Accuracy_RF[k,1] = sum(diag(T_RF))/sum(T_RF)
  k=k+1
}
print(sum(Accuracy_RF)/1000)
sd(as.vector(Accuracy_RF))
proc.time() - ptm
confusionMatrix(final.table.RF)
outputFile = "Accuracy_RF.csv"
write.table(Accuracy_RF, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_RF.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of Random Forests accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_RF.jpg")
dev.off();

# SVM "linear" 
B = 1000
k=1
final.table.SVM.L = matrix(0, nrow = 4, ncol = 4)
Accuracy_SVM_linear = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_SVM_linear) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_SVM_linear = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
  svm.mod=svm(train,Class.train, kernel ="linear")
  svm.prediction=predict(svm.mod,new = test,kernel ="linear")
  T_SVM_linear = T_SVM_linear + table(Class.test,svm.prediction)
  final.table.SVM.L = final.table.SVM.L + table(Class.test,svm.prediction)
  Accuracy_SVM_linear[k,1] = sum(diag(T_SVM_linear))/sum(T_SVM_linear)
  k=k+1}
print(sum(Accuracy_SVM_linear)/1000)
sd(as.vector(Accuracy_SVM_linear))
proc.time() - ptm
confusionMatrix(final.table.SVM.L)
outputFile = "Accuracy_SVM_linear.csv"
write.table(Accuracy_SVM_linear, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_SVM_linear.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of linear SVM accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_SVM_L.jpg")
dev.off();

# SVM "polynomial" 
B = 1000
k=1
final.table.SVM.P = matrix(0, nrow = 4, ncol = 4)
Accuracy_SVM_polynomial = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_SVM_polynomial) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_SVM_polynomial = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
  svm.mod = svm(train,Class.train,kernel ="polynomial",gamma=.18,degree=2,coef0=2.5)
  svm.prediction = predict(svm.mod,new = test,kernel ="polynomial",gamma=.18,degree=2,coef0=2.5)
  T_SVM_polynomial = T_SVM_polynomial + table(Class.test,svm.prediction)
  final.table.SVM.P = final.table.SVM.P + table(Class.test,svm.prediction)
  Accuracy_SVM_polynomial[k,1] = sum(diag(T_SVM_polynomial))/sum(T_SVM_polynomial)
  k=k+1
}
print(sum(Accuracy_SVM_polynomial)/1000)
sd(as.vector(Accuracy_SVM_polynomial))
proc.time() - ptm
confusionMatrix(final.table.SVM.P)
outputFile = "Accuracy_SVM_polynomial.csv"
write.table(Accuracy_SVM_polynomial, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_SVM_polynomial.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of polynomial SVM accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_SVM_P.jpg")
dev.off();

# SVM "radial" 
B = 1000
k=1
final.table.SVM.R = matrix(0, nrow = 4, ncol = 4)
Accuracy_SVM_radial = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_SVM_radial) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_SVM_radial = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
  svm.mod=svm(train,Class.train,kernel ="radial",gamma=.2)
  svm.prediction=predict(svm.mod,new=test,kernel ="radial",gamma=.2)
  T_SVM_radial = T_SVM_radial + table(Class.test,svm.prediction)
  final.table.SVM.R = final.table.SVM.R + table(Class.test,svm.prediction) 
  Accuracy_SVM_radial[k,1] = sum(diag(T_SVM_radial))/sum(T_SVM_radial)
  k=k+1
}
print(sum(Accuracy_SVM_radial)/1000)
sd(as.vector(Accuracy_SVM_radial))
proc.time() - ptm
confusionMatrix(final.table.SVM.R)
outputFile = "Accuracy_SVM_radial.csv"
write.table(Accuracy_SVM_radial, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_SVM_radial.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of radial SVM accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_SVM_R.jpg")
dev.off();

# SVM "sigmoid" 
B = 1000
k=1
final.table.SVM.S = matrix(0, nrow = 4, ncol = 4)
Accuracy_SVM_sigmoid = matrix(0, nrow = 1000, ncol = 1)
colnames(Accuracy_SVM_sigmoid) = c("Iterations")

ptm <- proc.time()

for(i in 1:B){
  load(sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))
  T_SVM_sigmoid = matrix(0, nrow = 4, ncol = 4)
  Class.train = train[,1]
  Class.test = test[,1]
  mean.train = mean(train[,2:13])
  std.train = sd(train[,2:13])
  train = scale(train[,2:13])
  test =  test[,2:13]
    for (p in 1:12){
      test[,p] = (test[,p]-mean.train[p])/std.train[p]}
  svm.mod=svm(train,Class.train,kernel ="sigmoid",gamma=.05,coef0=0)
  svm.prediction=predict(svm.mod,new=test,kernel ="sigmoid",gamma=.05,coef0=0)
  T_SVM_sigmoid = T_SVM_sigmoid + table(Class.test,svm.prediction)
  final.table.SVM.S = final.table.SVM.S + table(Class.test,svm.prediction)
  Accuracy_SVM_sigmoid[k,1] = sum(diag(T_SVM_sigmoid))/sum(T_SVM_sigmoid)
  k=k+1
}
print(sum(Accuracy_SVM_sigmoid)/1000)
sd(as.vector(Accuracy_SVM_sigmoid))
proc.time() - ptm
confusionMatrix(final.table.SVM.S)
outputFile = "Accuracy_SVM_sigmoid.csv"
write.table(Accuracy_SVM_sigmoid, file = outputFile,sep = ",")
X11()
accuracy = read.table("Accuracy_SVM_sigmoid.csv",sep=",",header=T)
accuracy = accuracy$Iterations
hist(accuracy,breaks = 20, main = "Histogram of sigmoid SVM accuracy", xlab = "Accuracy",prob=T)
lines(density(accuracy,adjust = 2),lwd = 3, col = "red")
dev.copy(jpeg, file = "C:/Users/user/Documents/Hist_boot_SVM_S.jpg")
dev.off();