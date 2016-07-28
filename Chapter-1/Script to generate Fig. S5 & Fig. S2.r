sensor=read.table("dw_data.csv",sep=",",header=T)
colnames(sensor) = c("Class","mobility J49","'off' current J49","'on' resistance J49", "mobility OMe", "'off' current OMe", "'on' resistance OMe", "mobility JM116","'off' current JM116","'on' resistance JM116","'on' resistance PTAA","'off' current PTAA","mobility PTAA")
rf.model = randomForest(sensor[,2:13],sensor[,1], importance=T, proximity=T)
varImpPlot(rf.model,main="Random Forests Variable Importance")

plsda.model = plsda(sensor[,2:13],sensor[,1],ncomp=12)
plot(plsda.model$loadings[,c(1,2)], type="n")
text(plsda.model$X[,x(1,2)], rownames(plsda.model$X))
