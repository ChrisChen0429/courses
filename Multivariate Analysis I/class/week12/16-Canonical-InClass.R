# Read data from 16-CanonicalCorrelation.csv

# data
(d = read.csv("16-CanonicalCorrelation.csv"))

# Sample covariance matrix:
(S = cov(d))


# Correlation matrixL
(R = cor(d))

# Let's test if Reading and Relaxation are independent:

# Test statistic:
n = nrow(d)
(t.test = R[1,3] * sqrt((n-2)/(1-R[1,3]^2)))

# p-value:
2*(1 - pt(t.test, n-2))
# Do not reject Ho: Reading and Relaxation are independent

# Built-in function:
cor.test(d$Relaxation, d$Reading)

# Notice the same result is obtained by testing if the regression slope is 0:
summary(lm(Reading ~ Relaxation, data = d))
# The t-statistic and p-value are equivalent!

# Let's test if cor(Relaxation, Reading) = 0.1
rho0 = 0.1
(z.test = (0.5*log((1 + R[1,3])/(1 - R[1,3]) - 0.5*log((1 + rho0)/(1 - rho0))))/sqrt(1/(n-3)))
# p-value:
2*(1 - pnorm(abs(z.test)))
# Do not reject the null, so rho could be 0.1


## Partial correlation between Reading and Relaxation given Motivation

# Manual calculation:

# Given
ind1 = c(2)
# Correlation
ind2 = c(1, 3)

(S2.1 = S[ind2, ind2] - S[ind2, ind1] %*% solve(S[ind1, ind1]) %*% S[ind1, ind2])

D = diag(sqrt(diag(S2.1)))
DInv = solve(D)
(R2.1 = DInv %*% S2.1 %*% DInv)
# -0.12

# With package ppcor
library(ppcor)
# all partial correlations
pcor(d)

# Partial between Reading and Relaxation given Motivation
pcor(d[,1:3])

# Is the partial correlation between Reading and Relaxation given Motivation significant?
# p-value = 0.33903109, so it isn't

# Multiple correlation between Reading and (Relaxation, Motivation)
# Manual calculation:
# Given
ind1 = c(1,2)

# Correlation
ind2 = c(3)

(r3.12 = sqrt( S[ind2, ind1] %*% solve(S[ind1, ind1]) %*% S[ind1, ind2] / S[ind2, ind2]))
# 0.3

# With lm function:
sqrt(summary(lm(Reading ~ Relaxation + Motivation, data = d))$r.squared)

# F-statistic:
p = 2
((n-p-1)/p * r3.12^2/(1-r3.12^2))
# Or
summary(lm(Reading ~ Relaxation + Motivation, data = d))$fstatistic


## CCA
# X1
ind1 = c(1,2)
# X2
ind2 = c(3:5)

(b.mat = solve(S[ind2, ind2]) %*% S[ind2, ind1] %*% solve(S[ind1, ind1]) %*% S[ind1, ind2]) 
sqrt(round(eigen(b.mat)$values, 5))
eigen(b.mat)$vectors



(a.mat = solve(S[ind1, ind1]) %*% S[ind1, ind2] %*% solve(S[ind2, ind2]) %*% S[ind2, ind1]) 
sqrt(round(eigen(a.mat)$values, 5))
eigen(a.mat)$vectors

# Different solution (as in the textbook):
S22 = S[ind2, ind2]
e <- eigen(S22)
V <- e$vectors

S22.sqrt = V %*% diag(sqrt(e$values)) %*% t(V)
(b.mat = solve(S22.sqrt) %*% S[ind2, ind1] %*% solve(S[ind1, ind1]) %*% S[ind1, ind2] %*% solve(S22.sqrt)) 
sqrt(round(eigen(b.mat)$values, 5))
solve(S22.sqrt) %*% eigen(b.mat)$vectors 

# The resulting canonical variates have unit variance:
var(as.matrix(d[,3:5]) %*% (solve(S22.sqrt) %*% eigen(b.mat)$vectors) [,1] )

# Built-in function
library(CCA)

(correl = matcor(d[,1:2], d[,3:5]))
img.matcor(correl, type = 2)

# Canonical correlations
cc(d[,1:2], d[,3:5])$cor

# Or
cancor(d[,1:2], d[,3:5])$cor

cc(d[,1:2], d[,3:5])$ycoef
cc(d[,1:2], d[,3:5])$xcoef
# Notice these match the textbook formulae


# Exercise:
# Read datasaet from T10_1_CHEM.DAT
d <- as.matrix(read.table("T10_1_CHEM.DAT"))
colnames(d) = c("Number", "Unchanged", "Converted", "Unwanted", "Temperature", "Concentration", "Time")

# X1 varaibles "Temperature", "Concentration", "Time" with interaction effects and quadratic terms
X1 <- cbind(d[, 5:7], d[,5]*d[,6], d[,5]*d[,7], d[,6]*d[,7], d[, 5:7]^2)
colnames(X1) = c("Temperature", "Concentration", "Time", "Temp.Conc", "Temp.Time", "Conc.Time", "Temp.Sq", "Conc.Sq", "Time.Sq")
X1

# X2 variables "Unchanged", "Converted", "Unwanted"
X2 <- d[, 2:4]

# a) Find all correlations between X1 and X2 

# b) Find the canonical correlations
# c) Find the coefficients of U and V variates for all canonical correlations
# USE cc function fro the package CCA!!
