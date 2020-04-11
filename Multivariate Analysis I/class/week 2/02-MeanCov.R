# Exercise 3.20 on p. 148
# Reading the data from table T3.2
# File T3-2.dat
# x1 = duration of snoq storm
# x2 = number of hours spent cleaning the snow
d = read.table(file.choose())
colnames(d) = c("x1", "x2")
d
attach(d)

# Defining the matrix X
X = as.matrix(d)
X

# Scatterplot
plot(x1, x2)


# Computing the means
x.bar <- c(mean(X[,1]), mean(X[,2]))
# or
colMeans(X)

abline (h = x.bar[2], lty = 2)
abline (v = x.bar[1], lty = 2)

# Covariance matrix
S <- cov(X)
S

# Correlation matrix
R = cor(X)
R

# Checking formula from lectures:
n = nrow(X)
j = rep(1, n)

# The mean vector formula:
t(X)%*%j/n

# Covariance matrix formula:
J = j %*% t(j)
I = diag(n)

(1/(n-1)) *t(X) %*% (I - J/n) %*% X

# Generalized var:
det(S)

# Total var:
sum(diag(S))



# Find the sample meand and variance of x2 - x1

# Method 1: Direct computation:
mean(x2 - x1)
var(x2 - x1)

# Method 2: Using the linear transformation formula:

C = c(-1, 1)
t(C) %*% x.bar
t(C) %*% S %*% C


# Exercise: Use the data in T1-11, which represents size (in acres) and number of visitors (in millions) at 
# the 15 most visited national parks in 2005 (p. 47)

# a) Calculate the covariance matrix S
# b) Find the generalized variance
# c) Find the diagonal matrix D with sample standard deviations on the main diagonal
# d) Find the correlation matrix R
# e) Verify that R = D^(-1/2) S D^(-1/2) (Eq. 3.29)
# f) Create a scatterplot. Identify the unusual park (btw, it is Great Smoky Mountain)
# g) Remove the unusual park and recalculate the correlation. What do you notice?
