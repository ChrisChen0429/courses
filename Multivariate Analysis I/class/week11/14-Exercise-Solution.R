## Exercise 9.26 on p. 537
# Use the mice weight data covariance matrix from Example 8.6
(R = matrix(c(1, 0.7501, 0.6329, 0.6363,
              0.7501, 1, 0.6925, 0.7386,
              0.6329, 0.6925, 1, 0.6625,
              0.6363, 0.7386, 0.6625, 1), 4, 4))
s = c(32.9909, 33.5918, 36.5534, 37.3517)
D = diag(s)
(S = D %*% R %*% D)


# d) Extra credit: Report the value of lambda_42 after rotation of PC solution with m = 2
# That is, the loading of the 4th variable on the 2nd factor after rotation




# a) Obtain the principal component solution for factor models with m = 1 and m = 2.

library(psych)
(mice.fa = principal(S, nfactors = 1, rotate = 'none', covar = TRUE))
(mice.fa = principal(S, nfactors = 2, rotate = 'none', covar = TRUE))

# b) Obtain the maximum likelihood solution for factor model with m = 1 
# Report the estimated loadings and specific variances
(fit = factanal(covmat = S, factors = 1, rotation = "none"))
loadings(fit)[1:4,]
fit$uniquenesses

# c) Perform a varimax rotation to the solution of part a) with m = 2

(mice.fa = principal(S, nfactors = 2, rotate = "varimax", covar = TRUE))

