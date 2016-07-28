rm(list=ls(all=TRUE))
library(graphics)
library(mclust)
library(chemometrics)
library(cluster)
library(clusterSim)

data=read.table("dw_data.csv",sep=",",header=T)
colnames(data) = c("Class","mobility J49","'off' current J49","'on' resistance J49", "mobility OMe", "'off' current OMe", "'on' resistance OMe", "mobility JM116","'off' current JM116","'on' resistance JM116","'on' resistance PTAA","'off' current PTAA","mobility PTAA")

# splitdf function will return a list of training and testing sets
splitdf<- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index<- 1:nrow(dataframe)
    trainindex<- sample(index, trunc(length(index)/2))
    trainset<- dataframe[trainindex, ]
    testset<- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}

#apply the function
split=splitdf(data,seed=1000)

# there are 64 and 63 observations in each data frame
lapply(split,nrow)

#view the first few columns in each data frame
lapply(split,head)

# save the training and testing sets as data frames
data.training<-split$trainset
data.testing<-split$testset

names.trn=(data.training[,1])
names.tst=(data.testing[,1])
data.trn=scale(data.training[,2:13])
data.tst=scale(data.testing[,2:13])

table(names.trn)

data.ldamod=lda(data.trn,grouping=names.trn,prior=rep(1,4)/4)
###########
data.ldamod$scaling
data.ldamod.plot=data.trn %*% data.ldamod$scaling

###########
u = c("acetone","DMMP","methanol","propanol")
X=as.vector(names.trn)

X[which(X == u[1])] = 0
X[which(X == u[2])] = 5
X[which(X == u[3])] = 1
X[which(X == u[4])] = 2

X = as.numeric(X)

u = c("acetone","DMMP","methanol","propanol")
Y=as.vector(names.tst)

Y[which(Y == u[1])] = 15
Y[which(Y == u[2])] = 18
Y[which(Y == u[3])] = 16
Y[which(Y == u[4])] = 17

Y = as.numeric(Y)
##########
plot(data.ldamod.plot,col=names.trn,pch=X,cex=1.7,main="(A)           Linear Discriminant Analysis")
###########

data.lda.testpred=predict(data.ldamod,new=data.tst)
table(names.tst,data.lda.testpred$class)

###test error###
test.error=1-sum(data.lda.testpred$class==names.tst)/length(names.trn)
print(sprintf("Test error = %f",test.error),quote=F)

points(data.lda.testpred$x,col=names.tst,pch=Y,cex=1.7)
#text(data.lda.testpred$x,labels=substring(names.tst,1,1),pos=1)

legend("bottomleft",c("acetone","DMMP","methanol","propanol"),cex=1,pch=c(0,5,1,2),
col=c("black","red","green","blue"),title="Test set")
legend("topleft",c("acetone","DMMP","methanol","propanol"),cex=1,pch=c(15,18,16,17),
col=c("black","red","green","blue"),title="Train set")


#plot3d(data.lda.testpred$x,size=2,type="s",col=data.lda.testpred$class)

#plot(data.ldamod,abbrev=T)#abbreviate names

###### ADDITIONAL PLOTS #####

#plot(data.ldamod,abbrev=T,dimen=1)
#plot histogram for each class
#plot(data.ldamod,abbrev=T,dimen=1,type="density")
#plot density curve for each class

rm(list=ls(all=TRUE))
library(graphics)
library(mclust)
library(chemometrics)
library(cluster)
library(clusterSim)

data=read.table("dw_data.csv",sep=",",header=T)
colnames(data) = c("Class","mobility J49","'off' current J49","'on' resistance J49", "mobility OMe", "'off' current OMe", "'on' resistance OMe", "mobility JM116","'off' current JM116","'on' resistance JM116","'on' resistance PTAA","'off' current PTAA","mobility PTAA")
data.ldamod=lda(scale(data[,2:13]),data[,1])
plot(data.ldamod$scaling[,c(1,2)],type="p",pch=19,main="(B) LDA loadings plot",xlim=c(-4,3))
text(data.ldamod$scaling[,c(1,2)], rownames(data.ldamod$scaling),pos=c(2,2,1,4,2,2,2,2,2,4,4,2))