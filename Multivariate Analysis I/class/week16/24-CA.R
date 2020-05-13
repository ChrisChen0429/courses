# Example 4.6.1 on p. 130 of Everitt & Hothorn
teensex = matrix(c(21, 8, 2, 21, 9, 3, 14, 6, 4, 13, 8, 10, 8, 2, 10), nrow = 3)
rownames(teensex) <- c("No boyfriend", "Boyfriend no sex", "Boyfriend sex")
colnames(teensex) <- c("<16", "16-17", "17-18", "18-19", "19-20")

# Contingecny table
(teen.table = as.table(teensex, dnn = c("Boyfriend", "Age group")))

# Table to data frame (if needed)
teensex <- as.data.frame(teen.table)
names(teensex) <- c("Boyfriend", "Age", "Freq")
teensex

# Data frame to contigency table
teensex <- xtabs(Freq ~ Boyfriend + Age, data = teensex)
teensex

# Marginal probabilities (masses)
n = sum(teensex)
(r = rowSums(teensex)/n)

# Age group
c = colSums(teensex)/n
c

# Joint probabilities aka frequencies
(P = prop.table(teen.table))

# Cross-check
teen.table/n

# Conditional probabilities (profiles) given the rows:
(R = prop.table(teen.table, margin = 1))

# Conditional given columns
C = prop.table(teen.table, margin = 2)
C


# Visualization:
library(graphics)
mosaicplot(teen.table, main = "Mosaic Plot of Teen Sex")
# Widths are proportional to marginal probabilities r
# Heights are proportional to conditional probabilities R
# If there is independence bars should be approximately the same

# Same applies to the other conditional given columns:
mosaicplot(t(teen.table), xlab = "Age Group")

# Conclusion: The two variables seem to be dependent.

# CA with the package
install.packages("ca")
library(ca)

fit = ca(teen.table)

plot(fit) # symmetric map
library(factoextra)
fviz_ca_biplot(fit, repel = TRUE)
# the standard plot of correspondence analysis is a symmetric biplot in which both 
# rows (blue points) and columns (red triangles) are represented in the same space 
# using the principal coordinates. 
# Rows or columns close together have similar profiles
# With symmetric plot, the inter-distance between rows and columns can’t be interpreted.

plot(fit, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map 
fviz_ca_biplot(fit, 
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
# To make an asymetric biplot, rows (or columns) points are plotted from the 
# standard coordinates and the profiles of the columns (or the rows) are plotted from 
# the principale coordinates  

# If the angle between two arrows is acute, 
# then their is a strong association between the corresponding row and column.


## Algebra:
# Matrix formula for conditional given rows:
solve(diag(r)) %*% P

# Matrix formula for conditional given columns
P%*%solve(diag(c))

# Testing independence with the Chi-square test:
(chisq = chisq.test(teen.table))

# Warning because there are some expected counts < 5
chisq$expected

# In such case we can compute p-value with a Monte Carlo simulation:
chisq.test(teen.table, simulate.p.value = T)

# Conclusion: Since p-value < 0.05 reject the H0 of independence
# Matches with our visual interpretation

# Test statistic value
chisq$statistic

# Cross-check with algebraic formula:
n*sum(diag(solve(diag(r)) %*% (P - r%*%t(c)) %*% solve(diag(c)) %*% t(P - r%*%t(c))))


## Row and column profiles

Z = sqrt(solve(diag(r))) %*% (P - r%*%t(c))   %*% sqrt(solve(diag(c)))
Z
# Cross-check
chisq$residuals/sqrt(n)

# Eigenvalues
(lambda.sq = round(eigen(t(Z) %*% Z)$values, 6))

# Contributions of frist 2:
lambda.sq[1]/sum(lambda.sq)
lambda.sq[2]/sum(lambda.sq)

# Eigenvectors for SVD:
k = min(nrow(teen.table)-1, ncol(teen.table)-1)
k
U = eigen(Z %*% t(Z))$vector[,1:k]
V = eigen(t(Z) %*% Z)$vector[, 1:k]
L = diag(lambda.sq)

# Coordinates:
A = sqrt(diag(r)) %*% U
A
B = sqrt(diag(c)) %*% V
(X = solve(diag(r)) %*% A %*% sqrt(L[1:k, 1:k]))

# Cross-checking with the package:
print(fit) # basic results
summary(fit) # extended results


# Example 2: Household tasks
library("factoextra")
data(housetasks)
housetasks
# The data is a contingency table containing 13 housetasks and their repartition in the couple

install.packages("gplots")
library("gplots")

# convert the data as a table
dt <- as.table(as.matrix(housetasks))
# Visual of the contingency table
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="", label = FALSE)

# Chi-square test:
chisq = chisq.test(housetasks)
chisq

# Alternative package:
install.packages("FactoMineR")
library(FactoMineR)
CA(X, ncp = 5, graph = TRUE)
res.ca = CA(housetasks, graph = TRUE)
#  housetasks such as dinner, breakfeast, laundry are done more often by the wife
# Driving and repairs are done by the husband 

print(res.ca)
summary(res.ca)
# Contains the dimension coordinates, contribution of rows and quality of representation