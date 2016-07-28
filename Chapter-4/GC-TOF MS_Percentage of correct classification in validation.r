comp = read.table("GC-TOF MS comparison.csv",sep=",",header=T)

plot(comp[,2],xlim=c(1,26),ylim=c(40,100),type = "n",lwd=2, lab = c(15,10,7),ylab = "Prediction accuracy (%)", xlab = "Number of components",cex.lab = 1.4,cex.main = 1.4,cex.main=1.4, main="(B) GC-TOF MS Arabidopsis thaliana data")
points(comp[,2],type = "l",lty = 1,lwd=2, col = "black")
points(comp[,3],type = "l",lty = 2,lwd=2, col = "red")
points(comp[,4],type = "l",lty = 3,lwd=2, col = "green")
points(comp[,5],type = "l",lty = 4,lwd=2, col = "blue")
points(comp[,6],type = "l",lty = 5,lwd=2, col = "purple")
points(comp[,7],type = "l",lty = 6,lwd=2, col = "brown")
legend("bottomright", c("Autoscaling","Range","Level","Pareto","Vast","None"),lwd=2, cex = 1, lty=1:6, col = c("black","red","green","blue","purple","brown"),title="Scaling")

