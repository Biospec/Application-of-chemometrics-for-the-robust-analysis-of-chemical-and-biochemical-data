library(mixOmics)
data=read.table("dw_data.csv",sep=",",header=T)
data.sc=scale(data[,2:13])
classes=data[,1]
data.df=data.frame(classes,data.sc)
colnames(data.df) = c("Class","mobility J49","'off' current J49","'on' resistance J49", "mobility OMe", "'off' current OMe", "'on' resistance OMe", "mobility JM116","'off' current JM116","'on' resistance JM116","'on' resistance PTAA","'off' current PTAA","mobility PTAA")
data.plsda=plsda(data.df[,2:13],data.df[,1],ncomp=12)
col <- as.numeric(as.factor(classes))
pch = as.numeric(as.factor(classes))
####### SCORES PLOT ######
plot(data.plsda$scores, col = col,pch=pch,main="(A) PLS-DA scores plot",cex=1.5)
legend("topleft", c("acetone", "DMMP", "methanol","propanol"), cex = 1, pch = 1:4, col = 1:4)

####### LOADNIGS PLOT ########
plot(data.plsda$loadings[,c(1,2)],type="n",xlim=c(-0.6,0.6),main="(B) PLS-DA loadings plot")
text(data.plsda$loadings[,c(1,2)], rownames(data.plsda$loadings))


