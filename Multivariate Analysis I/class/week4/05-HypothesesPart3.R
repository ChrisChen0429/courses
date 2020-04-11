

# Example 2 from last time:
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


a <- solve(Sp)%*%(x1.bar - x2.bar)
a

# y1=pictorial inconsistencies and y3=tool recognition contribute the most to separation of the genders


# Scaled version of Hotelling's T^2 that has F distribution
p <- ncol(X1)
((n1+n2-p-1)/((n1+n2-2)*p))*T.sq.test
# Compare to F(p, n1+n2-p-1) distribution
# p-value
pf(((n1+n2-p-1)/((n1+n2-2)*p))*T.sq.test, p ,n1+n2-p-1,lower.tail=FALSE)
# Since p-value < 0.05 we reject H0


# 95% CI for mu1-mu2
qf(0.95,p, n1 + n2 - p -1)
(c.sq = (n1 + n2 - 2)*p/(n1+ n2 -p - 1)*qf(0.95,p, n1 + n2 - p -1))

# mu11 - mu21:
(x1.bar[1] - x2.bar[1]) - sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[1,1])
(x1.bar[1] - x2.bar[1]) + sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[1,1])
# 95% CI is (1.44, 5.81) which doesn't contain 0 so difference in Var1 is significant

# mu12 - mu22:
(x1.bar[2] - x2.bar[2]) - sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[2,2])
(x1.bar[2] - x2.bar[2]) + sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[2,2])
# 95% CI is (-1.25, 5.25) which does contain 0 so difference in Var2 is  not significant

# mu13 - mu23:
(x1.bar[3] - x2.bar[3]) - sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[3,3])
(x1.bar[3] - x2.bar[3]) + sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[3,3])
# 95% CI is (6.12, 14.95) which doesn't contain 0 so difference in Var3 is significant

# mu14 - mu24:
(x1.bar[4] - x2.bar[4]) - sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[4,4])
(x1.bar[4] - x2.bar[4]) + sqrt(c.sq)*sqrt((1/n1 + 1/n2)*Sp[4,4])
# 95% CI is (-3.04, 4.66) which does contain 0 so difference in Var4 is  not significant



# Exercise 6.1 on p. 276

x <- read.table("T6-1.DAT")
x
# V1 = BOD in commercial lab
# V2 = SS in commercial lab
# V3 = BOD in state lab
# V4 = SS in state lab

(d = cbind(x$V1 - x$V3, x$V2 - x$V4))
(dbar = colMeans(d))

T.sq = nrow(d) * t(dbar) %*% solve(cov(d)) %*% dbar
T.sq

10*2*qf(0.95, 2, 9)/9
