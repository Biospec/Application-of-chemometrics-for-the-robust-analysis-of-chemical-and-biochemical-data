rm(list=ls(all=T))
############################ DATA PREPERATION ##################################
#load libraries
library(caret)
library(randomForest)
# Bootstrapping - data preperation phase 1 (based on 100 iterations)
# taking into account that user can be replace with any name depending on the computer used
mydata=read.table("dw_data.csv",sep=",",header=T)
n=length(mydata[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train=mydata[index,]
test=mydata[-index,]
save(train,test,file = sprintf("C:/Users/Piotr/Documents/DW_data/data%d.RData",i))}

########################### PARAMETERS SELECTION ###############################

rm(list=ls(all=TRUE))
B = 100
gamma = seq(0.18,0.22,0.01)
degree = seq(1,3,1)
coef0 = seq(2,2.5,0.1)

res = matrix(0, nrow = (length(gamma)*length(degree)*length(coef0)), ncol = 4)
colnames(res) = c("gamma", "degree", "coef0","accuracy")

k = 1
ptm <- proc.time()
for (g in 1:length(gamma))
{
    for (d in 1:length(degree))
    {
        for (c in 1:length(coef0))
        {
            final.table = matrix(0, nrow = 4, ncol = 4)
            misclassification.error = 0
            for (i in 1:B)
            {
                load(sprintf("C:/Users/Piotr/Documents/DW_data/data%d.RData",i))
                
      Class.train = train[,1]
      Class.test = test[,1]

      mean.test = mean(train[,2:13])
      std.test = sd(train[,2:13])

      train = scale(train[,2:13])

      test = test[,2:13]
        for (p in 1:12){
          test[,p]=(test[,p]-mean.test[p])/std.test[p]
        }
                
                svm.mod=svm(train,Class.train,kernel ="polynomial",gamma=gamma[g],degree=degree[d],coef0=coef0[c])
                svm.prediction=predict(svm.mod,new=test,kernel ="polynomial",gamma=gamma[g],degree=degree[d],coef0=coef0[c])
                #tmp=table(test[,1],svm.prediction)
                final.table = final.table + table(Class.test,svm.prediction)


            }

            res[k,1] = gamma[g]
            res[k,2] = degree[d]
            res[k,3] = coef0[c]
            res[k,4]=sum(diag(final.table))/sum(final.table)
            k=k+1
        }
    }
    print(g)
    flush.console()
}
proc.time() - ptm
confusionMatrix(final.table)
outputFile= "polynomial.test3.csv"
write.table(res, file = outputFile)
write.table(res,file="polynomial.test3.csv",sep = ",", col.names = colnames(res))