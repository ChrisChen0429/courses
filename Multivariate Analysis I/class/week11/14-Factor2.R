# Read the stock dataset from Table 8.4
stocks = as.matrix(read.table("T8-4.DAT"))
colnames(stocks) = c("JPM", "C", "WFC", "RDS", "XOM")


library(psych)
# The principal function performs factor analysis with the principal component method:
(stocks.fa = principal(stocks, nfactors = 2, rotate = 'none'))
# For example, JPM has communality of h1.sq = 0.73

# Residual matrix:
stocks.fa$residual
# Notice the specific variances (errors) are included on the diagonal

# Specific variances psi, called uniquenesses
stocks.fa$uniquenesses
# We can subtract them from the diagonal of the residual matrix:
stocks.fa$residual - diag(stocks.fa$uniquenesses)
# Now this matches with bottom of p. 493


# factanal function uses the MLE method:
(fit = factanal(stocks, factors = 2, rotation = "none"))
# For example, now the JPM communality h1.sq is  0.121^2 + 0.754^2 = 0.58
# Can also be obtained this way:
1-fit$uniquenesses

# Loadings:
(L = loadings(fit)[1:5,])

# Residual matrix:
round(cor(stocks) - L %*% t(L) - diag(fit$uniquenesses), 3)
# Much smaller than with PC method!!

# Test statistic:
fit$STATISTIC
# p-value:
fit$PVAL
# Conclusion: Do not reject H0. That is 2 factors are adequate!

# Rotated loadings:
(fit2 = factanal(stocks, factors = 2))

# Loadings after rotation
(L.rot = loadings(fit2)[1:5,])

# Specific variances Psi after rotation
(Psi.rot = diag(fit2$uniquenesses))
# Note the psis are the same as before rotation!

# Estimated values of the factors are called factor scores
# They are often used for diagnostics

# Scores with regression method:
(fit2 = factanal(stocks, factors = 2, scores = "regression"))
fit2$scores

# Scores with WLS method:
(fit3 = factanal(stocks, factors = 2, scores = "Bartlett"))
fit3$scores

# Scatterplot of the regression scores
plot(fit2$scores, pch = 19)
abline(h = 0, v = 0)
# Same as Figure 9.4 on p. 518

# We can also examine the scatteplot of scores obtained with different methods:
(stocks.fa2 = principal(stocks, nfactors = 2))
par(mfrow = c(1,2))
plot(fit2$scores[,1], stocks.fa2$scores[,1], xlab = "MLE", ylab = "PC", main = "Factor 1")
abline(0, 1)
plot(fit2$scores[,2], stocks.fa2$scores[,2], xlab = "MLE", ylab = "PC", main = "Factor 2")
abline(0, 1)
par(mfrow = c(1,1))
# We expect to see a strong fit around the 45 degree line
# If not, then this suggests we have too many factors and the last one is not needed


# Example 2:
# Data from file dataset_EFA.csv
# sample of 300 responses on 6 items from a survey of college students’ favorite subject 
# The items range in value from 1 to 5, which represent a scale from Strongly Dislike to 
# Strongly Like. Our 6 items asked students to rate their liking of different college 
# subject areas, including biology (BIO), geology (GEO), chemistry (CHEM), algebra (ALG), 
# calculus (CALC), and statistics (STAT).

my.data = read.csv(file.choose())
head(my.data)

n.factors = 2   

fit <- factanal(my.data, 
                n.factors,               
                scores=c("regression"),
                rotation="none")

print(fit, digits=2, cutoff=.3, sort = TRUE)
# sort will group together the variables in factors

head(fit$scores)

# plot factor 1 by factor 2 
load = fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(my.data),cex=.7) # add variable names

# With rotation for easier understanding
(fit = factanal(my.data, 
                n.factors,             
                rotation="varimax"))    # 'varimax' is an orthogonal rotation
# Total % explained variance is 66.5%

(load <- fit$loadings[,1:2] )

plot(load,type="n") # set up plot 
text(load,labels=names(my.data),cex=.7) # add variable names
# Interpretation:
# We could rename PA1 as Science, and PA2 as Math.

# We can try correlated factors, since STAT might share something with Science factor.
# This is done with oblique rotation:
library(psych)
(solution = fa(r = cor(my.data), nfactors = 2, rotate = "oblimin", fm = "pa"))
# Notice factors have 0.21 correlation

plot(solution,labels=names(my.data),cex=.7, ylim=c(-.1,1)) 


solution$complexity
# Hoffman's index of complexity for each variable
# Measures non-zero entries in the loadings for each variable
# ((Σ l_i^2)^2)/(Σ l_i^4), where l_i are the loadings of ith variable
# For example, for BIO:
load <- solution$loadings[,1:2]
sum(load[1,]^2)^2/sum(load[1,]^4)
# Average is reported in the summary

# Determining the number of factors:
install.packages("psy")
library(psy)

scree.plot(fit$correlation)

# Package nFactors offers more advanced techniques

install.packages("nFactors")
library(nFactors)

## Parallel Analysis of a Correlation Matrix
# Parallel analysis is a method for determining the number of components or factors 
# to retain from pca or factor analysis. The method works by creating a random dataset 
# with the same number of observations and variables as the original data. 
# A correlation matrix is computed from the randomly generated dataset and then 
# eigenvalues of the correlation matrix are computed. When the eigenvalues from the 
# random data are larger then the eigenvalues from the pca or factor analysis 
# you known that the components or factors are mostly random noise.

ap = parallel(subject=nrow(my.data),var=ncol(my.data), rep=100)
# This function gives the distribution of the eigenvalues of correlation 

(nS <- nScree(x = eigen(cor(my.data))$values, aparallel=ap$eigen$qevpea, model = "factors"))
# returns an analysis of the number of component or factors to retain
# Different solutions are given.
# The classical onesare the Kaiser rule, the parallel analysis, and the scree test 
# The Kaiser rule is to drop all components with eigenvalues under 1.0 
# eigen$qevpea is the quantile of the eigenvalues distribution
# Components$no is Number of factors to retain according to optimal coordinates oc

plotnScree(nS)
# Two components are greater that the distribution mean of 0
# Parallel analysis green triangle line crosses the solid line before 3rd comonent
# All methods agree we should use 2 factors!


## Exercise 9.26 on p. 537
# Use the mice weight data covariance matrix from Example 8.6
(R = matrix(c(1, 0.7501, 0.6329, 0.6363,
              0.7501, 1, 0.6925, 0.7386,
              0.6329, 0.6925, 1, 0.6625,
              0.6363, 0.7386, 0.6625, 1), 4, 4))
s = c(32.9909, 33.5918, 36.5534, 37.3517)
D = diag(s)
(S = D %*% R %*% D)



# a) Obtain the principal component solution for factor models with m = 1 and m = 2.
# b) Obtain the maximum likelihood solution for factor model with m = 1 
# Report the estimated loadings and specific variances
# c) Perform a varimax rotation to the solution of part a) with m = 2