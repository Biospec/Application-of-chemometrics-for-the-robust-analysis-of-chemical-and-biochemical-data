a = as.data.frame(LDA[1:48])
a$density <- sm.density(a$LDA[1:48],eval.points=a$LDA[1:48])$estimate
a.id <- order(a$LDA[1:48])
b = as.data.frame(LDA[49:132])
b$density <- sm.density(b$LDA[1:84],eval.points=b$LDA[1:84])$estimate
b.id <- order(b$LDA[1:84])
hist(a$LDA[1:48],freq=F,xlim=c(-6,4),col="red",main = "None scaling", xlab="")
hist(b$LDA[1:84],add=T,freq=F,col=rgb(0, 1, 0, 0.5))
lines(a$LDA[1:48][a.id],a$density[a.id],col="darkred",lwd=3)
lines(b$LDA[1:84][b.id],b$density[b.id],col="darkgreen",lwd=3)