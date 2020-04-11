# Moose example
# Read in the data from moose example
d = as.matrix(read.table(file ="moose.txt", sep = "\t", header = T))
d = d[, 2:4]

cov(d)

eigen(cov(d))


# Fowl example:
R = matrix(c(1, 0.584, 0.615, 0.601, 0.57, 0.6, 
             0.584, 1, 0.576, 0.53, 0.526, 0.555,
             0.615, 0.576, 1, 0.94, 0.875, 0.878,
             0.601, 0.53, 0.94, 1, 0.877, 0.886,
             0.57, 0.526, 0.875, 0.877, 1, 0.924,
             0.6, 0.555, 0.878, 0.886, 0.924, 1), 6, 6)

eigen(R)
eigenvec = eigen(R)$vectors
rownames(eigenvec) = c("Skull.Length", "Skull.Breadth", "Wing.Humerus", "Wing.Ulna", "Leg.Femur", "Leg.Tibia")
round(eigenvec, 2)


# Sweat example
# Read in the data from Table 5.1 (sweat data)
d = as.matrix(read.table(file.choose()))
colnames(d) = c("Sweat.rate", "Sodium", "Potassium")
d

# There is a reasonable amount of correlation among the variables:
pairs(d)
cor(d)

# Covariance matrix
(S <- cov(d))

# Eigenvalues
eigen(S)$values

# Proportion of explained variance with using the first eigenvalue:
eigen(S)$values[1]/sum( eigen(S)$values)
# More than enough!

# Eigenvectors:
C = eigen(S)$vectors
C

# The first two principal components are:
(a1 <- C[,1])
(a2 <- C[,2])

# Transformed data:
y1 = d %*% a1
y2 = d %*% a2

# Notice now data are uncorrelated 
round(cor(y1, y2), 3)

# Check variance:
var(y1)
# Exactly equal to:
eigen(S)$value[1]

# Check correlation with variables:
cor(y1, d[,1])
# Should be equal to:
eigen(S)$vectors[1,1] * sqrt(eigen(S)$values[1]) / sqrt(S[1,1])

cor(y1,d[,2])

# Using the built-in function:
prcomp(d)
# Note the result are the standard deviations of the PCs, that is, sqrt(lambda)
# Also note, PCs can be multiplied by -1 and still mean the same
C

# Checking for outliers:
qqnorm(y1)
qqline(y1)
plot(y1, y2)
y1
# Observation # 17 has the highest PC1 value, which is a bit unusual
# It is an individual with very low seat rate and sodium