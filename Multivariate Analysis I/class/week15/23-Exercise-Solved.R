## Exercsise:
# Use the eurodist built-in dataset
library(MASS)
eurodist

# a) Use cmdscale to obtain 2-dimensional MDS analysis. 
# Plot the results with city names as labels.
(mds <- cmdscale(eurodist))

plot(mds[,1], (-1)*mds[,2])
text(mds[, 1], (-1)*mds[, 2], labels = rownames(mds), cex = 0.6, pos = 1)
dist(mds)

# b) Use isoMDS with k = 1, 2 and 3
isoMDS(eurodist, k = 1)
isoMDS(eurodist, k = 2)
isoMDS(eurodist, k = 3)
