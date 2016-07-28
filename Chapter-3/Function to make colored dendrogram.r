#### Make sure that all libraries are loaded some of them will be automatically installed 
#### click yes to accept the labrary


####################
## Getting the data:

sample = data.frame(matrix(floor(abs(rnorm(20000)*100)),ncol=200))
groupCodes <- c(rep("Cont",25), rep("Tre1",25), rep("Tre2",25), rep("Tre3",25))
rownames(sample) <- make.unique(groupCodes)

colorCodes <- c(Cont="red", Tre1="green", Tre2="blue", Tre3="yellow")

distSamples <- dist(sample)
hc <- hclust(distSamples)
dend <- as.dendrogram(hc)

####################
## installing dendextend for the first time:

if (!require('installr')) install.packages('installr'); require('installr')
## install.Rtools() # run this if you are using Windows and don't have Rtools
require2(devtools)
install_github('dendextend', 'talgalili')
require2(Rcpp)
install_github('dendextendRcpp', 'talgalili')

####################
## Solving the question:

# loading the package
require(dendextend)
# Assigning the labels of dendrogram object with new colors:
labels_colors(dend) <- colorCodes[groupCodes][order.dendrogram(dend)]
# Plotting the new dendrogram
plot(dend)


####################
## A sub tree - so we can see better what we got:
par(cex = 1)
plot(dend[[1]], horiz = TRUE)