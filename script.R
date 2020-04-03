#!change working directory!
#Data Inladen


wijn <- read.csv("wijn.csv", header=FALSE, sep = ",")
summary(wijn)

#Data schalen
wijnScaled = scale(wijn[,2:13])

#ongeschaalde PCA maken
wijn.pca = pprcomp(wijn)

#geschaalde PCA maken
wijnScaled.pca = prcomp(wijnScaled,scale=TRUE)