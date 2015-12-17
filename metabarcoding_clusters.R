### Ascript for comparing compositions of metabarcoded samples ###########

### clear the work space
rm(list=ls())

### Hierarchical Cluster Analysis
# Select the variables that you want to base your clustering on
communities <- read.csv(file="Data/otu_percent_table.csv", header = T)
rownames(communities)<-communities$sample
communities<-communities[,2:2400]

# First, calculate a matrix of distances
d <-  dist(communities[,3:2399], method = "euclidean")
# See ?dist for alternative methods for generating the distance matrix

# Then, carry out the hierarchical cluster analysis
HC1 <- hclust(d, method = "ward.D2")
HC1

lab.col<-c("red","green","blue","black","purple","brown")[match(HC1$Tree,c("ASH","BEE","BET","control","HAZ","OAK"))]

# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- colLab<-c("red","green","blue","black","purple","brown")[match(HC1$Tree,c("ASH","BEE","BET","control","HAZ","OAK"))]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

# using dendrapply
clusDendro = dendrapply(HC1, colLab)
# make plot
plot(clusDendro, main = "Cool Dendrogram", type = "triangle")

# See ?hclust for explanations of alternative clustering methods and additional arguments.

# Plot the "dendrogram" (tree diagram)
plot(HC1, cex = 0.5, col.lab=HC1$lab.col)

# Cut the tree into several clusters (four clusters in this example)
clusters <- cutree(HC1, k = 4)
clusters

# Re-draw the dendrogram with blue lines defining the four clusters
rect.hclust(HC1, k=4, border = "blue")