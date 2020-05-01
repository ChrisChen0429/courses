# k-means partitioning

# Simulated data example
# Let's generate 3 groups from bivariate normal distribution:
library(MASS)
set.seed(439)
g1 = mvrnorm(n = 25, mu = c(10, 20), Sigma = matrix(c(6, -4, -4, 10), 2, 2))
plot(g1, pch = 19, col = "blue", xlim = c(0, 40), ylim = c(0, 30), xlab = "x", ylab = "y")

g2 = mvrnorm(n = 25, mu = c(20, 10), Sigma = matrix(c(6, -4, -4, 10), 2, 2))
points(g2, pch = 19)

g3 = mvrnorm(n = 25, mu = c(27, 20), Sigma = matrix(c(7, 4, 4, 12), 2, 2))
points(g3, pch = 19, col = "red")

# What the algorithm sees:
d = rbind(g1, g2, g3)
plot(d, pch = 19, col = "grey", cex = 0.9, xlab = "x", ylab = "y")

# Choose randomly 3 initial centers:
n = nrow(d)
(cen.ind = sample(1:n, 3))
(cen = d[cen.ind,])
points(cen, col =c ("black", "blue", "red"), pch = 2, cex = 1.5)

# Assign each observation to one of the 3 groups:
for (i in 1:n)
{
  # New data with row i and centers:
  d.cen = rbind(d[i,], cen)
  
  # Distance to each center:
  dist.cen = dist(d.cen)
  
  # Convert to matrix so we can use only specific column:
  dist.cen = as.matrix(dist.cen)
  
  if (which.min(dist.cen[1, 2:4]) == 1) points(d[i,1], d[i,2], pch = 2)
  if (which.min(dist.cen[1, 2:4]) == 2) points(d[i,1], d[i,2], pch = 2, col = "blue")
  if (which.min(dist.cen[1, 2:4]) == 3) points(d[i,1], d[i,2], pch = 2, col = "red")
}
# Note the colors don't matter because the group numbering is irrelevant

# Built-in function:
kmeans(d, cen)

# Iterations:
kmeans(d, cen)$iter

# Partitioning
kmeans(d, cen)$cluster

# Plot together the true groups with the clusters
par(mfrow = c(1, 2))
plot(d, pch = 19, col = "grey", cex = 0.9, xlab = "x", ylab = "y")
points(d[kmeans(d, cen)$cluster == 1,], pch = 2)
points(d[kmeans(d, cen)$cluster == 2,], pch = 2, col = "red")
points(d[kmeans(d, cen)$cluster == 3,], pch = 2, col = "blue")
# Again, colors are irrelevant, just grouping

# True groups:
plot(g1, pch = 19, col = "blue", xlim = c(0, 40), ylim = c(0, 30), xlab = "x", ylab = "y")
points(g2, pch = 19)
points(g3, pch = 19, col = "red")
par(mfrow = c(1, 1))
# Only one of the original blue points got missclassified!


## Example with Table 15.7 from Rencher
# Load file T15_7_PROTEIN.dat
d = read.table(file.choose(), header = F)
colnames(d) = c("Country", "RedMeat", "WhiteEggs", "Eggs", "Milk", "Fish", "Cereals", "StarchyFoods", "Nuts", "FruitVeg")
head(d)

# First let's try some hierarchical clustering:
# Single linkage
s.clust = hclust(dist(d[,2:10]), method = "single")
plot(s.clust, labels = d$Country)

# Complete linkage:
c.clust = hclust(dist(d[,2:10]))
plot(c.clust, labels = d$Country)

# Average linkage clustering
a.clust = hclust(dist(d[,2:10]), method = "average")
plot(a.clust, labels = d$Country)
# Eastern vs. Western bloc quite clear!

# Let's try with rescaling first:
a.clust = hclust(dist(scale(d[,2:10])), method = "average")
plot(a.clust, labels = d$Country)
# Makes more sense!

# Let's use 5 groups:
km = kmeans(scale(d[,2:10]), 5)
newd <- data.frame(d, km$cluster) 
newd[order(newd$km.cluster),]
# Mediterranean, Scandinavian, Balkans, Western, Central European

# Sum of squares:
km$withinss
km$betweenss

# MANOVA for testing the difference in clusters
d.sc = as.data.frame(scale(d[,2:10]))
dependent.vars = cbind(d.sc$RedMeat, d.sc$WhiteEggs, d.sc$Eggs, d.sc$Milk, d.sc$Fish, d.sc$Cereals, d.sc$StarchyFoods, d.sc$Nuts, d.sc$FruitVeg)
(ex.manova = summary(manova(dependent.vars ~ as.factor(km$cluster)), test = "Wilks"))
# There is a significant difference between the clusters!

ex.man = manova(dependent.vars ~ as.factor(km$cluster))
(summary.aov(ex.man))
# Cereals was the most significant

aggregate(d[, 7], list(km$cluster), mean)
newd[order(newd$km.cluster),]

# Discriminant analysis
newd.sc = data.frame(d.sc, km$cluster)
(ldamod = lda(km.cluster ~ ., data= newd.sc))
# LDA1: milk dominates
# LDA2: cereals vs. fruit and veggie
# LDA3: starchy foods
# LDA4: cereals + eggs

plot(ldamod)
newd

table(Predicted=predict(ldamod)$class, Actual= km$cluster)
# LDA achieves perfect separations


## Another example
df <- USArrests
df <- na.omit(df)
df <- scale(df)
head(df)

k2 <- kmeans(df, centers = 2, nstart = 25)
# nstart is how many starting points for the centers to be used
str(k2)
k2

library(factoextra) 
fviz_cluster(k2, data = df)
# Plots the clusters in the space of principal components.

k3 <- kmeans(df, centers = 3, nstart = 25)
fviz_cluster(k3, data = df)


## Wine data example:
data(wine, package='rattle')
head(wine)
# Notice we already have groups but will ignore them to see if cluster analysis works

# First standardize the variables
wine.stand <- scale(wine[,-1])

# Three groups (we are cheating here since we knew there are 3 types of wines)
k.means.fit <- kmeans(wine.stand, 3) 

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster
# Compare to true groups:
cbind(k.means.fit$cluster, wine$Type)
# Ignore the lables: perfect recovery!

# Plot to determing number of clusters based on Within Sum of Squares
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
    {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

wssplot(wine.stand, nc=6) 
# Look for "elbow"
# 3 clusters seems a good choice

# Another plot:
library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE, labels=2, lines=0)