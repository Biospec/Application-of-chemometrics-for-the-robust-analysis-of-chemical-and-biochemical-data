rm(list=ls(all=T))

NMR.diabetes=read.table("NMR.data.test.csv",sep=",",header=T)

n=length(NMR.diabetes[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train=NMR.diabetes[index,]
test=NMR.diabetes[-index,]
save(train,test,file = sprintf("C:/Users/Piotr/Documents/NMR.diabetes/data%d.RData",i))}