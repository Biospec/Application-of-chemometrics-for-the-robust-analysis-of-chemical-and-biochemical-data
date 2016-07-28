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