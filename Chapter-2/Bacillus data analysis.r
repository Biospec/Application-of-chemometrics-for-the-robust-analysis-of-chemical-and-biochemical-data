rm(list=ls(all=T))

# Bootstrapping - data preperation phase 1 (based on 100 iterations)
# taking into account that user (here Piotr) can be replace with any name
# depending on the computer used
Bacillus=read.table("bacillus.data.csv",sep=",",header=T)
Bacillus=Bacillus[,-2]
n=length(Bacillus[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train=Bacillus[index,]
test=Bacillus[-index,]
save(train,test,file = sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))}

### data preperation for validation again absed on 100 iterations
for(i in 1:100){
load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
p=length(train[,1])
new.index=sample(1:p,p,replace=T)
new.train=train[new.index,]
new.test=train[-new.index,]
save(new.train,new.test,file = sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))}

#### Prior to posterior analysis load the lybraries that are going to be used
library(MASS)
library(base)
library(pls)
library(e1071)
library(caret)
library(randomForest)
library(klaR)

rm(list=ls(all=T)) # to clear workspace from any values that might disturb the analysis

################################################################################
######################## VARIABLE SELECTION ####################################
################################################################################

################### LDA variable selection #####################################

# greedy.wilks - Performs a stepwise forward variable/model selection using the
# Wilk's Lambda criterion. A stepwise forward variable selection is performed.
# The initial model is defined by starting with the variable which separates the
# groups most. The model is then extended by including further variables depending
# on the Wilk's lambda criterion: Select the one which minimizes the Wilk's lambda
# of the model including the variable if its p-value still shows statistical significance.

B = 100

LDA.results = vector("list",B)

for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]

  new.train = scale(new.train[,3:152],scale=T)

  dat = data.frame(Class.train,new.train)
  bacillus.forward <- greedy.wilks(Class.train ~ .,data=dat, niveau = 0.001)
  bacillus.lda.fwd <- lda(bacillus.forward$formula, data = dat)
  LDA.var = Mod(bacillus.lda.fwd$scaling)
  colnames(LDA.var)=NULL
  LDA.results[[i]] = as.data.frame(t(LDA.var))
  print(i)
  flush.console()
}


var.sel = data.frame()
for(i in seq(along=LDA.results)) for(j in names(LDA.results[[i]]))
var.sel[i,j] = LDA.results[[i]][j]
var.sel

write.table(var.sel, file = "LDA.var.sel.csv",sep = ",")

votes = apply(var.sel,2, function(x)length(which(!is.na(x))))
LDA.votes.sorted = sort(votes,decreasing = T)
write.table(LDA.votes.sorted, file = "LDA.var.sel.votes.sorted.csv",sep = ",")

## additional analysis that has not been included in the paper

colMeans(var.sel, na.rm = T, dims = 1)
LDA.col.mean = sort(colMeans(var.sel, na.rm = T, dims = 1),decreasing=T)
write.table(LDA.col.mean, file = "LDA.col.mean.csv",sep = ",")

################### PLSDA variable selection ###################################

rm(list=ls(all=T))

B = 100
PLSDA.varImp = matrix(0,nrow=150,ncol=1)

PLSDA.results = vector("list",B)

for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]
  new.train = scale(new.train[,3:152],scale=T)
  rownames(PLSDA.varImp) = c(colnames(new.train))
  plsda.model = plsda(new.train,Class.train, ncomp = 15)
  PLSDA.varImp = varImp(plsda.model)

  Ordered.variable = PLSDA.varImp[order(-PLSDA.varImp$Overall),,drop=F]
  PLSDA.results[[i]] = as.data.frame(t(rownames(Ordered.variable)))

  print(i)
  flush.console()
}
PLSDA.ranked.variables = t(sapply(PLSDA.results, unlist))
write.table(PLSDA.ranked.variables, file = "PLSDA.var.sel.csv",sep = ",")
PLSDA.ranked.variables.30 = sort(table(PLSDA.ranked.variables[,1:30]),decreasing=T)
write.table(PLSDA.ranked.variables.30, file = "PLSDA.ranked.variables.30.csv",sep = ",")

PLSDA.ranked.variables.15 = sort(table(PLSDA.ranked.variables[,1:15]),decreasing=T)
write.table(PLSDA.ranked.variables.15, file = "PLSDA.ranked.variables.15.csv",sep = ",")

PLSDA.ranked.variables.10 = sort(table(PLSDA.ranked.variables[,1:10]),decreasing=T)
write.table(PLSDA.ranked.variables.10, file = "PLSDA.ranked.variables.10.csv",sep = ",")

PLSDA.ranked.variables.5 = sort(table(PLSDA.ranked.variables[,1:5]),decreasing=T)
write.table(PLSDA.ranked.variables.5, file = "PLSDA.ranked.variables.5.csv",sep = ",")


################### SVM-RFE variable selection #################################

rm(list=ls(all=T))

# R implementation of the SVM-RFE algorithm for binary classification problems
# The next function implements the SVM-RFE algorithm as it is described in by Guyon.
# The R implementation of the LIBSVM library, library(e1071), was used to for this implementation.
# The SVM-RFE algorithm proposed by Guyon returns a ranking of the features of a
# classification problem by training a SVM with a linear kernel and removing the feature
# with smallest ranking criterion. This criterion is the w value of the desicion hiperplane
# given by the SVM.

svmrfeFeatureRanking = function(x,y){
 n = ncol(x)
 survivingFeaturesIndexes = seq(1:n)
 featureRankedList = vector(length=n)
 rankedFeatureIndex = n
 while(length(survivingFeaturesIndexes)>0){
 #train the support vector machine
 svmModel = svm(x[, survivingFeaturesIndexes], y, cost = 10, cachesize=500,
scale=F, type="C-classification", kernel="linear" )
 #compute the weight vector
 w = t(svmModel$coefs)%*%svmModel$SV
 #compute ranking criteria
 rankingCriteria = w * w
 #rank the features
 ranking = sort(rankingCriteria, index.return = TRUE)$ix
 #update feature ranked list
 featureRankedList[rankedFeatureIndex] = survivingFeaturesIndexes[ranking[1]]
 rankedFeatureIndex = rankedFeatureIndex - 1
 #eliminate the feature with smallest ranking criterion
 (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1]])
 }
 return (featureRankedList)
}

B = 100
SVM.results = vector("list",B)
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  # restrict training examples to good feature indices
  y = new.train[,1]
  x = scale(new.train[,3:152],scale=T)
  ranking = svmrfeFeatureRanking(x,y)+50
  SVM.results[[i]] = as.data.frame(ranking)
  print(i)
  flush.console()
}
SVM.ranked.variables = t(sapply(SVM.results, unlist))
write.table(SVM.ranked.variables, file = "SVM.var.sel.csv",sep = ",")

SVM.ranked.variables.30 = sort(table(SVM.ranked.variables[,1:30]),decreasing=T)
write.table(SVM.ranked.variables.30, file = "SVM.ranked.variables.30.csv",sep = ",")

SVM.ranked.variables.15 = sort(table(SVM.ranked.variables[,1:15]),decreasing=T)
write.table(SVM.ranked.variables.15, file = "SVM.ranked.variables.15.csv",sep = ",")

SVM.ranked.variables.10 = sort(table(SVM.ranked.variables[,1:10]),decreasing=T)
write.table(SVM.ranked.variables.10, file = "SVM.ranked.variables.10.csv",sep = ",")

SVM.ranked.variables.5 = sort(table(SVM.ranked.variables[,1:5]),decreasing=T)
write.table(SVM.ranked.variables.5, file = "SVM.ranked.variables.5.csv",sep = ",")

######################  RF variable selection  #################################

rm(list=ls(all=T))

B=100
MDA=matrix(0,nrow=150,ncol=1)
MDG=matrix(0,nrow=150,ncol=1)
MDA.results = vector("list",B)
MDG.results = vector("list",B)
for(i in 1:B){
load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
data.rf=randomForest(train[,3:152],train[,1],importance=T,proximity=T)
MDA = as.data.frame(round(importance(data.rf,type=1,scale=T),2))
MDG = as.data.frame(round(importance(data.rf,type=2,scale=T),2))
MDA.results[[i]] = rownames(MDA[order(-MDA$MeanDecreaseAccuracy),,drop=F])
MDG.results[[i]] = rownames(MDG[order(-MDG$MeanDecreaseGini),,drop=F])
print(i)
flush.console()
}
MDA.ranked.variables = t(sapply(MDA.results, unlist))
MDG.ranked.variables = t(sapply(MDG.results, unlist))
write.table(MDA.ranked.variables, file = "MDA.ranked.variables.csv",sep = ",")
write.table(MDG.ranked.variables, file = "MDG.ranked.variables.csv",sep = ",")
## List top of the variables that has been included as the most important among top 30 for all 100 runs
MDA.ranked.variables.30 = sort(table(MDA.ranked.variables[,1:30]),decreasing=T)
write.table(MDA.ranked.variables.30, file = "MDA.ranked.variables.30.csv",sep = ",")
MDG.ranked.variables.30 = sort(table(MDG.ranked.variables[,1:30]),decreasing=T)
write.table(MDG.ranked.variables.30, file = "MDG.ranked.variables.30.csv",sep = ",")
## List top of the variables that has been included as the most important among top 15 for all 100 runs
MDA.ranked.variables.15 = sort(table(MDA.ranked.variables[,1:15]),decreasing=T)
write.table(MDA.ranked.variables.15, file = "MDA.ranked.variables.15.csv",sep = ",")
MDG.ranked.variables.15 = sort(table(MDG.ranked.variables[,1:15]),decreasing=T)
write.table(MDG.ranked.variables.15, file = "MDG.ranked.variables.15.csv",sep = ",")
## List top of the variables that has been included as the most important among top 10 for all 100 runs
MDA.ranked.variables.10 = sort(table(MDA.ranked.variables[,1:10]),decreasing=T)
write.table(MDA.ranked.variables.10, file = "MDA.ranked.variables.10.csv",sep = ",")
MDG.ranked.variables.10 = sort(table(MDG.ranked.variables[,1:10]),decreasing=T)
write.table(MDG.ranked.variables.10, file = "MDG.ranked.variables.10.csv",sep = ",")
## List top of the variables that has been included as the most important among top 5 for all 100 runs
MDA.ranked.variables.5 = sort(table(MDA.ranked.variables[,1:5]),decreasing=T)
write.table(MDA.ranked.variables.5, file = "MDA.ranked.variables.5.csv",sep = ",")
MDG.ranked.variables.5 = sort(table(MDG.ranked.variables[,1:5]),decreasing=T)
write.table(MDG.ranked.variables.5, file = "MDG.ranked.variables.5.csv",sep = ",")





################################################################################
#####  DATA ANALYSIS FOR CLASSES FOR ALL METHODS (VALIDATION AND TEST DATA) ####
################################################################################
rm(list=ls(all=T))
# LDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 5  - c(16,28,41,66,109) 
Accuracy_LDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_LDA_val = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = new.test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  lda.model = lda(new.train,Class.train)
  lda.prediction = predict(lda.model, new = new.test)
  final_table_class_LDA_val = final_table_class_LDA_val + table(Class.test,lda.prediction$class)
  Accuracy_LDA_val[k,1] = sum(diag(final_table_class_LDA_val))/sum(final_table_class_LDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_LDA_val)
#write.table(Accuracy_LDA_val, file = "Accuracy_LDA_val.csv",sep = ",")
rm(list=ls(all=T))
# LDA TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152 
# 5  - c(16,28,41,66,109)  
Accuracy_LDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_LDA_test = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
lda.model = lda(new.train,Class.train)
lda.prediction = predict(lda.model, new = test)
final_table_class_LDA_test = final_table_class_LDA_test + table(Class.test,lda.prediction$class)
Accuracy_LDA_test[k,1] = sum(diag(final_table_class_LDA_test))/sum(final_table_class_LDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_LDA_test)
#write.table(Accuracy_LDA_test, file = "Accuracy_LDA_test.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
#c(28,36,16,29,57) 
Accuracy_PLSDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_PLSDA_val = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = new.test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  plsda.model = plsda(new.train,Class.train,ncomp = 15)
  plsda.prediction = predict(plsda.model, new = new.test,ncomp = 15, type = "class")
  final_table_class_PLSDA_val = final_table_class_PLSDA_val + table(Class.test,plsda.prediction)
  Accuracy_PLSDA_val[k,1] = sum(diag(final_table_class_PLSDA_val))/sum(final_table_class_PLSDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_PLSDA_val)
#write.table(Accuracy_PLSDA_val, file = "Accuracy_PLSDA_val.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA test 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
#c(28,36,16,29,57) 
Accuracy_PLSDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_PLSDA_test = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
plsda.model = plsda(new.train,Class.train,ncomp = 15)
plsda.prediction = predict(plsda.model, new = test,ncomp = 15, type = "class")
final_table_class_PLSDA_test = final_table_class_PLSDA_test + table(Class.test,plsda.prediction)
Accuracy_PLSDA_test[k,1] = sum(diag(final_table_class_PLSDA_test))/sum(final_table_class_PLSDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_PLSDA_test)
#write.table(Accuracy_PLSDA_test, file = "Accuracy_PLSDA_test.csv",sep = ",")
rm(list=ls(all=T))
# RF VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152 
#c(57,28,29,16,15)
Accuracy_RF_val = matrix(0, nrow=100, ncol=1)
final_table_class_RF_val = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = new.test[,1]
  new.train = new.train[,variables]
  new.test =  new.test[,variables]    
  rf.model = randomForest(new.train,Class.train,importance=T, proximity=T)
  rf.prediction = predict(rf.model, new = new.test)
  final_table_class_RF_val = final_table_class_RF_val + table(Class.test,rf.prediction)
  Accuracy_RF_val[k,1] = sum(diag(final_table_class_RF_val))/sum(final_table_class_RF_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_RF_val)
#write.table(Accuracy_RF_val, file = "Accuracy_RF_val.csv",sep = ",")
rm(list=ls(all=T))
# RF TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152 
#c(57,28,29,16,15) 
Accuracy_RF_test = matrix(0,nrow=100,ncol=1)
final_table_class_RF_test = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = test[,1]
  new.train = new.train[,variables]
  test =  test[,variables]
rf.model = randomForest(new.train,Class.train,importance=T, proximity=T)
rf.prediction = predict(rf.model, new = test)
final_table_class_RF_test = final_table_class_RF_test + table(Class.test,rf.prediction)
Accuracy_RF_test[k,1] = sum(diag(final_table_class_RF_test))/sum(final_table_class_RF_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_RF_test)
#write.table(Accuracy_RF_test, file = "Accuracy_RF_test.csv",sep = ",") 
rm(list=ls(all=T))
# SVM VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1 
variables = 3:152
#c(57,16,28,106,36) 
Accuracy_SVM_val = matrix(0, nrow=100, ncol=1)
final_table_class_SVM_val = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = new.test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  svm.model = svm(new.train,Class.train,kernel = "linear")
  svm.prediction = predict(svm.model, new = new.test,kernel = "linear")
  final_table_class_SVM_val = final_table_class_SVM_val + table(Class.test,svm.prediction)
  Accuracy_SVM_val[k,1] = sum(diag(final_table_class_SVM_val))/sum(final_table_class_SVM_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_SVM_val)
#write.table(Accuracy_SVM_val, file = "Accuracy_SVM_val.csv",sep = ",")
rm(list=ls(all=T))
# SVM TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152 
#c(57,16,28,106,36) 
Accuracy_SVM_test = matrix(0,nrow=100,ncol=1)
final_table_class_SVM_test = matrix(0, nrow = 2, ncol = 2) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,1]
  Class.test = test[,1]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
svm.model = svm(new.train,Class.train,kernel = "linear")
svm.prediction = predict(svm.model, new = test,kernel = "linear")
final_table_class_SVM_test = final_table_class_SVM_test + table(Class.test,svm.prediction)
Accuracy_SVM_test[k,1] = sum(diag(final_table_class_SVM_test))/sum(final_table_class_SVM_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_SVM_test)
#write.table(Accuracy_SVM_test, file = "Accuracy_SVM_test.csv",sep = ",")






################################################################################
# DATA ANALYSIS OF COMBINED SPECIES FOR ALL METHODS (VALIDATION AND TEST DATA) #
################################################################################
rm(list=ls(all=T))
# LDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109) 
Accuracy_LDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_LDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = new.test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  lda.model = lda(new.train,Class.train)
  lda.prediction = predict(lda.model, new = new.test)
  final_table_class_LDA_val = final_table_class_LDA_val + table(Class.test,lda.prediction$class)
  Accuracy_LDA_val[k,1] = sum(diag(final_table_class_LDA_val))/sum(final_table_class_LDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_LDA_val)
#write.table(Accuracy_LDA_val, file = "Accuracy_LDA_val.csv",sep = ",")
rm(list=ls(all=T))
# LDA TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109)   
Accuracy_LDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_LDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
lda.model = lda(new.train,Class.train)
lda.prediction = predict(lda.model, new = test)
final_table_class_LDA_test = final_table_class_LDA_test + table(Class.test,lda.prediction$class)
Accuracy_LDA_test[k,1] = sum(diag(final_table_class_LDA_test))/sum(final_table_class_LDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_LDA_test)
#write.table(Accuracy_LDA_test, file = "Accuracy_LDA_test.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)  
Accuracy_PLSDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_PLSDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = new.test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  plsda.model = plsda(new.train,Class.train,ncomp = 15)
  plsda.prediction = predict(plsda.model, new = new.test,ncomp = 15, type = "class")
  final_table_class_PLSDA_val = final_table_class_PLSDA_val + table(Class.test,plsda.prediction)
  Accuracy_PLSDA_val[k,1] = sum(diag(final_table_class_PLSDA_val))/sum(final_table_class_PLSDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_PLSDA_val)
#write.table(Accuracy_PLSDA_val, file = "Accuracy_PLSDA_val.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA test 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)  
Accuracy_PLSDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_PLSDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
plsda.model = plsda(new.train,Class.train,ncomp = 15)
plsda.prediction = predict(plsda.model, new = test,ncomp = 15, type = "class")
final_table_class_PLSDA_test = final_table_class_PLSDA_test + table(Class.test,plsda.prediction)
Accuracy_PLSDA_test[k,1] = sum(diag(final_table_class_PLSDA_test))/sum(final_table_class_PLSDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_PLSDA_test)
#write.table(Accuracy_PLSDA_test, file = "Accuracy_PLSDA_test.csv",sep = ",")
rm(list=ls(all=T))
# RF VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# MDA
# 30 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51,106,31,71,56,23,3,13,22,79,88,18,11,38,37,116)
# 15 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51) 
# 10 - c(57,28,29,16,15,36,43,17,42,10) 
# 5  - c(57,16,28,106,36)
#MDG
# 30 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56,66,71,51,106,18,3,88,23,38,30,79,59,13,68,73)
# 15 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56) 
# 10 - c(57,16,28,106,36,17,42,43,36,41)
# 5  - c(57,16,28,106,36) 
Accuracy_RF_val = matrix(0, nrow=100, ncol=1)
final_table_class_RF_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = new.test[,2]
  new.train = new.train[,variables]
  new.test =  new.test[,variables]
  rf.model = randomForest(new.train,Class.train,importance=T, proximity=T)
  rf.prediction = predict(rf.model, new = new.test)
  final_table_class_RF_val = final_table_class_RF_val + table(Class.test,rf.prediction)
  Accuracy_RF_val[k,1] = sum(diag(final_table_class_RF_val))/sum(final_table_class_RF_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_RF_val)
#write.table(Accuracy_RF_val, file = "Accuracy_RF_val.csv",sep = ",")
rm(list=ls(all=T))
# RF TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152 
# MDA
# 30 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51,106,31,71,56,23,3,13,22,79,88,18,11,38,37,116)
# 15 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51) 
# 10 - c(57,28,29,16,15,36,43,17,42,10) 
# 5  - c(57,16,28,106,36)
#MDG
# 30 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56,66,71,51,106,18,3,88,23,38,30,79,59,13,68,73)
# 15 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56) 
# 10 - c(57,16,28,106,36,17,42,43,36,41)
# 5  - c(57,16,28,106,36) 
Accuracy_RF_test = matrix(0,nrow=100,ncol=1)
final_table_class_RF_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = test[,2]
  new.train = new.train[,variables]
  test =  test[,variables]
rf.model = randomForest(new.train,Class.train,importance=T, proximity=T)
rf.prediction = predict(rf.model, new = test)
final_table_class_RF_test = final_table_class_RF_test + table(Class.test,rf.prediction)
Accuracy_RF_test[k,1] = sum(diag(final_table_class_RF_test))/sum(final_table_class_RF_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_RF_test)
#write.table(Accuracy_RF_test, file = "Accuracy_RF_test.csv",sep = ",") 
rm(list=ls(all=T))
# SVM VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1 
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73) 
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5) 
# 10 - c(57,16,28,106,36,41,66,29,119,31) 
# 5  - c(57,16,28,106,36)
Accuracy_SVM_val = matrix(0, nrow=100, ncol=1)
final_table_class_SVM_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = new.test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  new.test =  new.test[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  svm.model = svm(new.train,Class.train,kernel = "linear")
  svm.prediction = predict(svm.model, new = new.test,kernel = "linear")
  final_table_class_SVM_val = final_table_class_SVM_val + table(Class.test,svm.prediction)
  Accuracy_SVM_val[k,1] = sum(diag(final_table_class_SVM_val))/sum(final_table_class_SVM_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_SVM_val)
#write.table(Accuracy_SVM_val, file = "Accuracy_SVM_val.csv",sep = ",")
rm(list=ls(all=T))
# SVM TEST 
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73) 
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5) 
# 10 - c(57,16,28,106,36,41,66,29,119,31) 
# 5  - c(57,16,28,106,36) 
Accuracy_SVM_test = matrix(0,nrow=100,ncol=1)
final_table_class_SVM_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix  
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i)) 
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = test[,2]
  mean.test = mean(new.train[,variables])
  std.test = sd(new.train[,variables])
  new.train = scale(new.train[,variables])
  test =  test[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }  
svm.model = svm(new.train,Class.train,kernel = "linear")
svm.prediction = predict(svm.model, new = test,kernel = "linear")
final_table_class_SVM_test = final_table_class_SVM_test + table(Class.test,svm.prediction)
Accuracy_SVM_test[k,1] = sum(diag(final_table_class_SVM_test))/sum(final_table_class_SVM_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_SVM_test)
#write.table(Accuracy_SVM_test, file = "Accuracy_SVM_test.csv",sep = ",")






################################################################################
#DATA ANALYSIS OF VEGETATIVE SPECIES FOR ALL METHODS (VALIDATION AND TEST DATA)#
################################################################################
rm(list=ls(all=T))
# LDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109)
Accuracy_LDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_LDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])
      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]
      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])
      new.train = scale(physio.st.train$vegetative[,variables])
      new.test = physio.st.test$vegetative[,variables]
        for (p in 1:length(new.test)){
          new.test[,p]=(new.test[,p]-mean.test[p])/std.test[p]
        }
  lda.model = lda(new.train,Class.train)
  lda.prediction = predict(lda.model, new = new.test)
  final_table_class_LDA_val = final_table_class_LDA_val + table(Class.test,lda.prediction$class)
  Accuracy_LDA_val[k,1] = sum(diag(final_table_class_LDA_val))/sum(final_table_class_LDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_LDA_val)
#write.table(Accuracy_LDA_val, file = "Accuracy_LDA_val.csv",sep = ",")
rm(list=ls(all=T))
# LDA TEST
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109)
Accuracy_LDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_LDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])
      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]

      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])

      new.train = scale(physio.st.train$vegetative[,variables])

      test = physio.st.test$vegetative[,variables]
        for (p in 1:length(test)){
          test[,p]=(test[,p]-mean.test[p])/std.test[p]
        }
lda.model = lda(new.train,Class.train)
lda.prediction = predict(lda.model, new = test)
final_table_class_LDA_test = final_table_class_LDA_test + table(Class.test,lda.prediction$class)
Accuracy_LDA_test[k,1] = sum(diag(final_table_class_LDA_test))/sum(final_table_class_LDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_LDA_test)
#write.table(Accuracy_LDA_test, file = "Accuracy_LDA_test.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)
Accuracy_PLSDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_PLSDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])

      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]

      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])

      new.train = scale(physio.st.train$vegetative[,variables])

      new.test = physio.st.test$vegetative[,variables]

        for (p in 1:length(new.test)){
          new.test[,p]=(new.test[,p]-mean.test[p])/std.test[p]
        }
  plsda.model = plsda(new.train,Class.train,ncomp = 15)
  plsda.prediction = predict(plsda.model, new = new.test,ncomp = 15, type = "class")
  final_table_class_PLSDA_val = final_table_class_PLSDA_val + table(Class.test,plsda.prediction)
  Accuracy_PLSDA_val[k,1] = sum(diag(final_table_class_PLSDA_val))/sum(final_table_class_PLSDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_PLSDA_val)
#write.table(Accuracy_PLSDA_val, file = "Accuracy_PLSDA_val.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA test
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)
Accuracy_PLSDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_PLSDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])

      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]

      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])

      new.train = scale(physio.st.train$vegetative[,variables])

      test = physio.st.test$vegetative[,variables]
      
      for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }
plsda.model = plsda(new.train,Class.train,ncomp = 15)
plsda.prediction = predict(plsda.model, new = test,ncomp = 15, type = "class")
final_table_class_PLSDA_test = final_table_class_PLSDA_test + table(Class.test,plsda.prediction)
Accuracy_PLSDA_test[k,1] = sum(diag(final_table_class_PLSDA_test))/sum(final_table_class_PLSDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_PLSDA_test)
#write.table(Accuracy_PLSDA_test, file = "Accuracy_PLSDA_test.csv",sep = ",")

rm(list=ls(all=T))
# SVM VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73)
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5)
# 10 - c(57,16,28,106,36,41,66,29,119,31)
# 5  - c(57,16,28,106,36)
Accuracy_SVM_val = matrix(0, nrow=100, ncol=1)
final_table_class_SVM_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])

      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]

      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])

      new.train = scale(physio.st.train$vegetative[,variables])

      new.test = physio.st.test$vegetative[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  svm.model = svm(new.train,Class.train,kernel = "linear")
  svm.prediction = predict(svm.model, new = new.test,kernel = "linear")
  final_table_class_SVM_val = final_table_class_SVM_val + table(Class.test,svm.prediction)
  Accuracy_SVM_val[k,1] = sum(diag(final_table_class_SVM_val))/sum(final_table_class_SVM_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_SVM_val)
#write.table(Accuracy_SVM_val, file = "Accuracy_SVM_val.csv",sep = ",")
rm(list=ls(all=T))
# SVM TEST
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73)
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5)
# 10 - c(57,16,28,106,36,41,66,29,119,31)
# 5  - c(57,16,28,106,36)
Accuracy_SVM_test = matrix(0,nrow=100,ncol=1)
final_table_class_SVM_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])

      Class.train = physio.st.train$vegetative[,2]
      Class.test = physio.st.test$vegetative[,2]

      mean.test = mean(physio.st.train$vegetative[,variables])
      std.test = sd(physio.st.train$vegetative[,variables])

      new.train = scale(physio.st.train$vegetative[,variables])

      test = physio.st.test$vegetative[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }
svm.model = svm(new.train,Class.train,kernel = "linear")
svm.prediction = predict(svm.model, new = test,kernel = "linear")
final_table_class_SVM_test = final_table_class_SVM_test + table(Class.test,svm.prediction)
Accuracy_SVM_test[k,1] = sum(diag(final_table_class_SVM_test))/sum(final_table_class_SVM_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_SVM_test)
#write.table(Accuracy_SVM_test, file = "Accuracy_SVM_test.csv",sep = ",")









################################################################################
## DATA ANALYSIS OF SPORES SPECIES FOR ALL METHODS (VALIDATION AND TEST DATA) ##
################################################################################
rm(list=ls(all=T))
# LDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109)
Accuracy_LDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_LDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      new.test = physio.st.test$spores[,variables]
        for (p in 1:length(new.test)){
          new.test[,p]=(new.test[,p]-mean.test[p])/std.test[p]
        }
  lda.model = lda(new.train,Class.train)
  lda.prediction = predict(lda.model, new = new.test)
  final_table_class_LDA_val = final_table_class_LDA_val + table(Class.test,lda.prediction$class)
  Accuracy_LDA_val[k,1] = sum(diag(final_table_class_LDA_val))/sum(final_table_class_LDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_LDA_val)
#write.table(Accuracy_LDA_val, file = "Accuracy_LDA_val.csv",sep = ",")
rm(list=ls(all=T))
# LDA TEST
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79,115,108,80,119,59,70,34,98,10,58,95,84,14,51,137)
# 15 - c(16,28,41,66,109,68,78,69,57,74,13,36,56,31,79)
# 10 - c(16,28,41,66,109,68,78,69,57,74)
# 5  - c(16,28,41,66,109)
Accuracy_LDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_LDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      test = physio.st.test$spores[,variables]
        for (p in 1:length(test)){
          test[,p]=(test[,p]-mean.test[p])/std.test[p]
        }
lda.model = lda(new.train,Class.train)
lda.prediction = predict(lda.model, new = test)
final_table_class_LDA_test = final_table_class_LDA_test + table(Class.test,lda.prediction$class)
Accuracy_LDA_test[k,1] = sum(diag(final_table_class_LDA_test))/sum(final_table_class_LDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_LDA_test)
#write.table(Accuracy_LDA_test, file = "Accuracy_LDA_test.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)
Accuracy_PLSDA_val = matrix(0, nrow=100, ncol=1)
final_table_class_PLSDA_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
     physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      new.test = physio.st.test$spores[,variables]

        for (p in 1:length(new.test)){
          new.test[,p]=(new.test[,p]-mean.test[p])/std.test[p]
        }
  plsda.model = plsda(new.train,Class.train,ncomp = 15)
  plsda.prediction = predict(plsda.model, new = new.test,ncomp = 15, type = "class")
  final_table_class_PLSDA_val = final_table_class_PLSDA_val + table(Class.test,plsda.prediction)
  Accuracy_PLSDA_val[k,1] = sum(diag(final_table_class_PLSDA_val))/sum(final_table_class_PLSDA_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_PLSDA_val)
#write.table(Accuracy_PLSDA_val, file = "Accuracy_PLSDA_val.csv",sep = ",")
rm(list=ls(all=T))
# PLSDA test
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106,72,3,44,95,88,22,56,18,79,9,12,68,71,30,54)
# 15 - c(28,36,16,29,57,17,10,31,41,66,15,43,42,51,106)
# 10 - c(28,36,16,29,57,17,10,31,41,66)
# 5  - c(28,36,16,29,57)
Accuracy_PLSDA_test = matrix(0,nrow=100,ncol=1)
final_table_class_PLSDA_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
     physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      test = physio.st.test$spores[,variables]
      for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }
plsda.model = plsda(new.train,Class.train,ncomp = 15)
plsda.prediction = predict(plsda.model, new = test,ncomp = 15, type = "class")
final_table_class_PLSDA_test = final_table_class_PLSDA_test + table(Class.test,plsda.prediction)
Accuracy_PLSDA_test[k,1] = sum(diag(final_table_class_PLSDA_test))/sum(final_table_class_PLSDA_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_PLSDA_test)
#write.table(Accuracy_PLSDA_test, file = "Accuracy_PLSDA_test.csv",sep = ",")
rm(list=ls(all=T))
# SVM VALIDATION
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73)
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5)
# 10 - c(57,16,28,106,36,41,66,29,119,31)
# 5  - c(57,16,28,106,36)
Accuracy_SVM_val = matrix(0, nrow=100, ncol=1)
final_table_class_SVM_val = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
     physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(new.test,new.test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      new.test = physio.st.test$spores[,variables]
    for (p in 1:length(new.test)){
      new.test[,p] = (new.test[,p]-mean.test[p])/std.test[p]
      }
  svm.model = svm(new.train,Class.train,kernel = "linear")
  svm.prediction = predict(svm.model, new = new.test,kernel = "linear")
  final_table_class_SVM_val = final_table_class_SVM_val + table(Class.test,svm.prediction)
  Accuracy_SVM_val[k,1] = sum(diag(final_table_class_SVM_val))/sum(final_table_class_SVM_val)
  k=k+1
  print(i)
  flush.console()
}
mean(Accuracy_SVM_val)
#write.table(Accuracy_SVM_val, file = "Accuracy_SVM_val.csv",sep = ",")
rm(list=ls(all=T))
# SVM TEST
B = 100 # number of bootstrap that we want to perform on selected data set
k=1
variables = 3:152
# 30 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5,10,95,51,56,68,22,59,79,98,115,13,86,103,131,73)
# 15 - c(57,16,28,106,36,41,66,29,119,31,42,15,17,43,5)
# 10 - c(57,16,28,106,36,41,66,29,119,31)
# 5  - c(57,16,28,106,36)
Accuracy_SVM_test = matrix(0,nrow=100,ncol=1)
final_table_class_SVM_test = matrix(0, nrow = 14, ncol = 14) # Table for confusion matrix
  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  physio.st.train = split(new.train, new.train[,1])
      physio.st.train = split(new.train, new.train[,1])
      physio.st.test = split(test,test[,1])

      Class.train = physio.st.train$spores[,2]
      Class.test = physio.st.test$spores[,2]

      mean.test = mean(physio.st.train$spores[,variables])
      std.test = sd(physio.st.train$spores[,variables])

      new.train = scale(physio.st.train$spores[,variables])

      test = physio.st.test$spores[,variables]
    for (p in 1:length(test)){
      test[,p]=(test[,p]-mean.test[p])/std.test[p]
      }
svm.model = svm(new.train,Class.train,kernel = "linear")
svm.prediction = predict(svm.model, new = test,kernel = "linear")
final_table_class_SVM_test = final_table_class_SVM_test + table(Class.test,svm.prediction)
Accuracy_SVM_test[k,1] = sum(diag(final_table_class_SVM_test))/sum(final_table_class_SVM_test)
k=k+1
print(i)
flush.console()
}
mean(Accuracy_SVM_test)
#write.table(Accuracy_SVM_test, file = "Accuracy_SVM_test.csv",sep = ",")



################################################################################
################################################################################
################# Random Forest (Spores/Vegetative) ############################
################################################################################
################################################################################

rm(list=ls(all=T))
# RF VAL
B = 100 
k=1
variables = 3:152
# MDA
# 30 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51,106,31,71,56,23,3,13,22,79,88,18,11,38,37,116)
# 15 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51)
# 10 - c(57,28,29,16,15,36,43,17,42,10)
# 5  - c(57,16,28,106,36)
#MDG
# 30 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56,66,71,51,106,18,3,88,23,38,30,79,59,13,68,73)
# 15 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56)
# 10 - c(57,16,28,106,36,17,42,43,36,41)
# 5  - c(57,16,28,106,36)
Accuracy_RF_veg_val = matrix(0,nrow=100,ncol=1)
Accuracy_RF_spo_val = matrix(0,nrow=100,ncol=1)
final_table_RF_val = matrix(0, nrow = 14, ncol = 14) 

for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = new.test[,2]
  new.train = new.train[,variables]
  new.test =  new.test[,variables]
rf.model = randomForest(new.train,Class.train, importance=T, proximity=T)
rf.prediction = predict(rf.model, new = new.test)
final_table_RF_val = final_table_RF_val + table(Class.test,rf.prediction)
Accuracy_RF_veg_val [k,1] = sum(diag(final_table_RF_val[1:7,1:7]))/sum(final_table_RF_val[1:7,1:7])
Accuracy_RF_spo_val [k,1] = sum(diag(final_table_RF_val[8:14,8:14]))/sum(final_table_RF_val[8:14,8:14])
k=k+1
print(i)
flush.console()
}
mean(Accuracy_RF_veg_val)
mean(Accuracy_RF_spo_val)


rm(list=ls(all=T))
# RF TEST
B = 100 
k=1
variables = 3:152
# MDA
# 30 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51,106,31,71,56,23,3,13,22,79,88,18,11,38,37,116)
# 15 - c(57,28,29,16,15,36,43,17,42,10,41,44,72,66,51)
# 10 - c(57,28,29,16,15,36,43,17,42,10)
# 5  - c(57,16,28,106,36)
#MDG
# 30 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56,66,71,51,106,18,3,88,23,38,30,79,59,13,68,73)
# 15 - c(57,16,28,106,36,17,42,43,36,41,72,44,10,31,56)
# 10 - c(57,16,28,106,36,17,42,43,36,41)
# 5  - c(57,16,28,106,36)
Accuracy_RF_veg_test = matrix(0,nrow=100,ncol=1)
Accuracy_RF_spo_test = matrix(0,nrow=100,ncol=1)
final_table_RF_test = matrix(0, nrow = 14, ncol = 14) 

  for(i in 1:B){
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase2/data%d.RData",i))
  load(sprintf("C:/Users/Piotr/Documents/B_data_prep_phase1/data%d.RData",i))
  Class.train = new.train[,2]
  Class.test = test[,2]
  new.train = new.train[,variables]
  test =  test[,variables]
rf.model = randomForest(new.train,Class.train, importance=T, proximity=T)
rf.prediction = predict(rf.model, new = test)
final_table_RF_test = final_table_RF_test + table(Class.test,rf.prediction)
Accuracy_RF_veg_test [k,1] = sum(diag(final_table_RF_test[1:7,1:7]))/sum(final_table_RF_test[1:7,1:7])
Accuracy_RF_spo_test [k,1] = sum(diag(final_table_RF_test[8:14,8:14]))/sum(final_table_RF_test[8:14,8:14])
k=k+1
print(i)
flush.console()
}
mean(Accuracy_RF_veg_test)
mean(Accuracy_RF_spo_test)
