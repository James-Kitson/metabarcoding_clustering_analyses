### Ascript for comparing compositions of metabarcoded samples ###########

### clear the work space
rm(list=ls())

### Hierarchical Cluster Analysis
# Select the variables that you want to base your clustering on
communities <- read.csv(file="Data/otu_percent_table.csv", header = T)

# First, calculate a matrix of distances
d <-  dist(dframe1, method = "euclidean")
# See ?dist for alternative methods for generating the distance matrix

# Then, carry out the hierarchical cluster analysis
HC1 <- hclust(d, method = "ward.D2")
HC1

# See ?hclust for explanations of alternative clustering methods and additional arguments.

# Plot the "dendrogram" (tree diagram)
plot(HC1, cex = 0.5)

# Cut the tree into several clusters (four clusters in this example)
clusters <- cutree(HC1, k = 4)
clusters

# Re-draw the dendrogram with blue lines defining the four clusters
rect.hclust(HC1, k=4, border = "blue")