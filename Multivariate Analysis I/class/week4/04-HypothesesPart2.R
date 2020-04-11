# Ex 5.3.2 on p. 133
# Read the data from Table 3.4
x <- read.table("T5-1.DAT")
x

# x-bars
(x.bar = colMeans(x))

# Mean under Ho
mu0 <- c(4, 50, 10) 
# Matches with p. 214 calculation

n <- nrow(x)
(T.sq.test <- n*t(x.bar - mu0) %*% solve(cov(x)) %*% (x.bar - mu0))

# Reference F:
p = ncol(x)
(n-1)*p*qf(0.9, p, n-p)/(n-p)

# Conclusion: Since T.sq.test > crit value, we reject Ho

# p-value using the F distribution equivalent from p. 132
# (n-p)/((n-1)*p) T.sq ~ F(p, n-p)

((n-p)/((n-1)*p))*T.sq.test
p.value <- pf(((n-p)/((n-1)*p))*T.sq.test, p, n-p,lower.tail=FALSE)
p.value


# Wilks' Lambda on page 217:
x = as.matrix(x)
denom = 0
for (i in 1:n) denom = denom + (x[i,]-mu0) %*% t(x[i,]-mu0)
denom = denom/(n-1)
cov(x)

(L = (det(cov(x))/det(denom))^(n/2))


# Compare to Hotelling T^2:
L^(2/n)
1/(1+T.sq.test/(n-1))
round(T.sq.test,5) == round((n-1)*det(denom)/det(cov(x)) - (n-1),5)
# They are the same!




# Example 2:
# Four psychological tests on 32 males and 32 females

# Read the dataset first
x <- read.table("T5_1_PSYCH.DAT")
x

# V1 = 1 means male
# Partitioning the date into two groups (males and females)
X1 <- as.matrix(x[1:32,2:5])
X2 <- as.matrix(x[33:64,2:5])
x1.bar <- apply(X1, 2, mean)
x2.bar <- apply(X2, 2, mean)
S1 <- cov(X1)
S2 <- cov(X2)
n1 = nrow(X1)
n2 = nrow(X2)
Sp <- (1/(n1 + n2 -2))*((n1-1)*S1 + (n2-1)*S2)
Sp
T.sq.test <- (n1*n2/(n1+n2))*t(x1.bar-x2.bar) %*% solve(Sp) %*% (x1.bar-x2.bar)
T.sq.test



# Scaled version of Hotelling's T^2 that has F distribution
p <- ncol(X1)
((n1+n2-p-1)/((n1+n2-2)*p))*T.sq.test
# Compare to F(p, n1+n2-p-1) distribution
# p-value
pf(((n1+n2-p-1)/((n1+n2-2)*p))*T.sq.test, p ,n1+n2-p-1,lower.tail=FALSE)
# Since p-value < 0.05 we reject H0






# Alternatively, use the ICSNP package
#install.packages("ICSNP")
library(ICSNP)
HotellingsT2(X1, X2)
# Note the output is already converted to F distribution

# You can use it also for the one-sample case:
# (Back to example 1)
x <- read.table("T5-1.DAT")

# First column is useless so we extract the data from columns 2-4
X <- as.matrix(x)

# x-bars
x.bar <- colMeans(X)

# Mean under Ho
mu0 <- c(4, 50, 10) 
HotellingsT2(X, mu = mu0)

# Results match manual computation!
