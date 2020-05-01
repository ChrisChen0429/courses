# Example 4.4.2 on p. 110 of Everitt & Hothorn
# Data matrix of dim. 10x5
X <- matrix(c(
  3, 5, 6, 1, 4, 2, 0, 0, 7, 2,
  4, 1, 2, 1, 7, 2, 4, 6, 6, 1,
  4, 1, 0, 1, 3, 5, 1, 4, 5, 4,
  6, 7, 2, 0, 6, 1, 1, 3, 1, 3,
  1, 3, 6, 3, 2, 0, 1, 5, 4, 1), nrow = 10)
X

(D <- dist(X))

# Classical Multidimensional Scaling:
cmdscale(D, k = 9, eig = TRUE)
# Tries to recover data X from the distance matrix D
# You can specify any dimension k up to N-1
# Note p = 5, N = 10, so we tried MDS with maximum k
# Eigenvalues are of the matrix B = XX' estimated by
# b_ij = -0.5(d^2_ij - d^_i - d^2_j + d^2)

# Check the MDS achieves complete recovery of the original distances when k =5:
cmdscale(D, k = 5)
# Note the "recovered" points are not the same as the original data X but distances are:
max(abs(dist(X) - dist(cmdscale(D, k = 5))))

# Check duality between MDS and PCA:
max(abs(prcomp(X)$x) - abs(cmdscale(D, k = 5)))
# Note classical MDS with Euclidean distances is equivalent to PCA on covariance matrix

# Non-Euclidean distances:
X_m <- cmdscale(dist(X, method = "manhattan"), k = nrow(X) - 1, eig = TRUE)

# Eigenvalues:
(X_eigen <- X_m$eig)
# Notice some negative eigenvalues since
# When the observed proximity matrix is not Euclidean, the matrix B is
# not positive-definite. 

# Applying criteria for choosing lower dimension:
cumsum(abs(X_eigen)) / sum(abs(X_eigen))
cumsum(X_eigen^2) / sum(X_eigen^2)
# Values above 0.8 are good

# First suggest 4
# Second suggests 3


# Example: fertility and socio-economic data on 47 French speaking provinces in Switzerland.
data("swiss")
head(swiss)

mds = cmdscale(dist(swiss) )
plot(as.matrix(mds))
text(as.matrix(mds)[,1], as.matrix(mds)[,2] , 
     rownames(mds), cex = 0.4,pos=1 )


# Data from Table 4.1 on p. 113 of Everitt and Hothorn:
# (Same as Ex 16.1.2 on p. 559 of Rencher)
# Airline distances:
# The book has a typo in the distance between Atlanta and Seattle!

"airline.dist" <-
  structure(.Data = list(c(0, 587, 1212, 701, 1936, 604, 748, 2139, 2182, 543)
                         , c(587, 0, 920, 940, 1745, 1188, 713, 1858, 1737, 597)
                         , c(1212, 920, 0, 879, 831, 1726, 1631, 949, 1021, 1494)
                         , c(701, 940, 879, 0, 1374, 968, 1420, 1645, 1891, 1220)
                         , c(1936, 1745, 831, 1374, 0, 2339, 2451, 347, 959, 2300)
                         , c(604, 1188, 1726, 968, 2339, 0, 1092, 2594, 2734, 923)
                         , c(748, 713, 1631, 1420, 2451, 1092, 0, 2571, 2408, 205)
                         , c(2139, 1858, 949, 1645, 347, 2594, 2571, 0, 678, 2442)
                         , c(218, 1737, 1021, 1891, 959, 2734, 2408, 678, 0, 2329)
                         , c(543, 597, 1494, 1220, 2300, 923, 205, 2442, 2329, 0)
  )
  , names = c("ATL", "ORD", "DEN", "HOU", "LAX", "MIA",    
              "JFK", "SFO", "SEA", "IAD")
  , row.names = c("ATL", "ORD", "DEN", "HOU", "LAX", "MIA",
                  "JFK", "SFO", "SEA", "IAD")     
  , class = "data.frame"
  )

airdist <- as.dist(as.matrix(airline.dist))
airdist


# MDS
airline_mds <- cmdscale(airdist, k = 9, eig = TRUE)
airline_mds$points

# Eigenvalues:
(lam <- airline_mds$eig)
# Note distances are not Euclidean so we have negative eigenvalues

# Checking adequacy:
cumsum(abs(lam)) / sum(abs(lam))
cumsum(lam^2) / sum(lam^2)
# m =2 is good

# Plot the MDS:
lim <- range(airline_mds$points[,1] * (-1)) * 1.2
plot(airline_mds$points[,1] * (-1), airline_mds$points[,2]*(-1) ,
     type = "n", xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = lim, ylim = lim)
text(airline_mds$points[,1] *(-1), airline_mds$points[,2]*(-1) , 
     labels(airdist), cex = 0.7)
# Notice how well the geographic location of the airports has been recovered!


## Voting example:
library("MASS")
install.packages("HSAUR2")
data("voting", package = "HSAUR2")
voting
# Romesburg (1984) gives a set of data that shows the number of times 15 congressmen 
# from NJ voted differently in the House of Representatives on 19 environmental bills.

# Kruskal's Non-metric Multidimensional Scaling
voting_mds <- isoMDS(voting)

x <- voting_mds$points[,1]
y <- voting_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(voting_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(voting), cex = 0.6)

# Shepard diagram
voting_sh <- Shepard(voting[lower.tri(voting)],
                     voting_mds$points)

plot(voting_sh, pch = 15, xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(voting_sh$x), 
     ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")
# A Shepard diagram plots MDS distances on the y-axis vs the input proximities 
# (/distances/similarities/dissimilarities) on the x-axis.

## Exercsise:
# Use the eurodist built-in dataset
eurodist

# a) Use cmdscale to obtain 2-dimensional MDS analysis. 
# Plot the results with city names as labels.

# b) Use isoMDS with k = 1, 2 and 3
