accuracy=read.table("Accuracy.RF.csv",sep=",",header=T)
acc=accuracy$Accuracy
hist(acc,main="Histogram of Random Forests accuracy",xlab="Accuracy")
lines(density(acc, adjust=1.9), lwd=3,col="red") 