rm(list=ls(all=T))

Kusano=read.table("AKI.data.csv",sep=",",header=T)

n=length(Kusano[,1])
for(i in 1:100){
index=sample(1:n,n,replace=T)
train=Kusano[index,]
test=Kusano[-index,]
save(train,test,file = sprintf("C:/Users/Piotr/Documents/AKI.data/data%d.RData",i))}