rm(list=ls(all=T))
# Bootstrapping - data preperation phase 1 (based on 1000 iterations)
# taking into account that user can be replace with any name depending on the computer used
sensor=read.table("dw_data.csv",sep=",",header=T)

n=length(sensor[,1])

# Data preperation of artificial independent data set (test set)
for(i in 1:1000){
index = sample(1:n,n,replace=T)
train = sensor[index,]
test = sensor[-index,]
save(train,test,file = sprintf("C:/Users/user/Documents/Sensor_data_prep/data%d.RData",i))}
