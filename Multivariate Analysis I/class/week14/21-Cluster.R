# Example (Public Utility Data). Read file T12-5.DAT
d = read.table("T12-5.DAT", header = F)
colnames(d) = c("CovRatio", "ROC", "CostPerKW", "Load", "DemandGrowth", "Sales", "Percent", "TotalCost","Company")
head(d)

# Eucledian distances
# Often it is advisable to standardize the variables so they all have unit variance
round(dist(scale(d[,1:8])), 2)
# Matches with the lecture notes and textbook

# Other distances:
round(dist(scale(d[,1:8]), method = "manhattan"), 2)

# Average linkage clustering
av.clust = hclust(dist(scale(d[,1:8])), method = "average")
plot(av.clust, labels = d$Company)

# Alternative plots:
#install.packages("ape")
library("ape")
# Default plot
plot(as.phylo(av.clust), cex = 0.6, label.offset = 0.5)

# Cladogram
plot(as.phylo(av.clust), type = "cladogram", cex = 0.6, 
     label.offset = 0.5)

# Unrooted
plot(as.phylo(av.clust), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

# Data in Table 15.1 from Rencher
# Read data from T15_1_CITYCRIME.dat
d = read.table("T15_1_CITYCRIME.dat", header = F)
colnames(d) = c("City", "Murder", "Rape", "Robbery", "Assault", "Burglary", "Larceny", "AutoTheft")
head(d)

# Eucledian distances
dist.matrix = as.matrix(dist(d[,2:8]))
colnames(dist.matrix) = d$City
rownames(dist.matrix) = d$City
round(dist.matrix, 1)

# Which are the two most similar cities?
which(dist.matrix == min(dist(d[,2:8])), arr.ind = T)

# Restart your R session here to restore the graphics because of the ape package

# Single linkage clustering
s.clust = hclust(dist(d[,2:8]), method = "single")
plot(s.clust, labels = d$City)
# Matches with Fig. 15.3 on p. 510 of Rencher

# Complete linkage clustering
s.clust = hclust(dist(d[,2:8]))
plot(s.clust, labels = d$City)
# Matches with Fig. 15.5 on p. 513

# Average linkage clustering
s.clust = hclust(dist(d[,2:8]), method = "average")
plot(s.clust, labels = d$City)
# Matches with Fig. 15.6 on p. 515

# Other linkage methods:
# Centroid linkage clustering
# Centroid-linkage is the distance between the centroids of two clusters.
s.clust = hclust(dist(d[,2:8]), method = "centroid")
plot(s.clust, labels = d$City)
# Note centroid linkage is not monotonic

# Median linkage clustering
s.clust = hclust(dist(d[,2:8]), method = "median")
plot(s.clust, labels = d$City)

# Ward linkage clustering
# Ward's minimum variance criterion minimizes the total within-cluster variance. 
s.clust = hclust(dist(d[,2:8]), method = "ward.D2")
plot(s.clust, labels = d$City)

# Exercise:
# Use the women track record data:
# Read the data from TrackWomen.dat
data <- read.table("TrackWomen.dat", sep="\t",header = T)
# DO NOT STANDARDIZE
# a) Obtain the distance matrix using Eucledian distance
a <- round(dist(data[,2:8]), 2)
min(a)

# b) Obtain the distance matrix using Manhattan distance
a <- round(dist(data[,2:8], method = "manhattan"), 2) 
a <- as.matrix(a)
which(a==0.48)

for (i in 1:54){
  for (j in a[i,]){
    if (j == 0.48){
      print(i)
    }
  }
}


# From now on USE EUCLIDEAN DISTANCE
# c) Perform single-linkage clustering and dendrogram
av.clust = hclust(dist(data[,2:8]), method = "single")
plot(av.clust, labels = data$Country)

# d) Perform complete-linkage clustering and dendrogram
av.clust = hclust(dist(data[,2:8]), method = "complete")
plot(av.clust, labels = data$Country)

# e) Perform average-linkage clustering and dendrogram
av.clust = hclust(dist(data[,2:8]), method = "average")
plot(av.clust, labels = data$Country)
