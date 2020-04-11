# Calculations for example 6.9

x1 = matrix(c(9, 6, 9, 3, 2, 7), 3, 2)
x2 = matrix(c(0, 2, 4, 0), 2, 2)
x3 = matrix(c(3, 1, 2, 8, 9, 7), 3, 2)
X = rbind(x1, x2, x3)

n1 = nrow(x1)
n2 = nrow(x2)
n3 = nrow(x3)
n = n1 + n2 + n3
g = 3

x1.bar = colMeans(x1)
x2.bar = colMeans(x2)
x3.bar = colMeans(x3)
x.bar = colMeans(X)

B = n1 * (x1.bar - x.bar) %*% t(x1.bar - x.bar) + n2 * (x2.bar - x.bar)%*%t(x2.bar - x.bar) + n3*(x3.bar - x.bar) %*% t(x3.bar - x.bar)
B

W  = (n1 - 1) * cov(x1) + (n2 - 1) * cov(x2) + (n3 - 1) * cov(x3)
W

(Lambda = det(W)/det(B + W))

# F-stat
(n - g - 1)/(g - 1) * (1 - sqrt(Lambda))/sqrt(Lambda)

# F-critical
alpha = 0.01
qf(1 - alpha, 2*(g-1), 2*(n - g - 1))
# Since F-Stat > F-crit reject Ho

# built-in function:
g = factor(c("g1 ,g1, g1, g2, g2, g3, g3, g3"))
d = cbind(X, g)

Data <- data.frame(X, Group = factor(rep(c("g1", "g2", "g3"), times=c(n1, n2, n3))))
colnames(Data) = c("V1", "V2", "Group")
Data

dependent.vars = cbind(Data$V1, Data$V2)
(ex.manova = summary(manova(dependent.vars ~ Data$Group), test = "Wilks"))

# Computation of B and W matrices


ex.manova

ex.manova$SS

# B matrix
ex.manova$SS[1]

# W matrix
ex.manova$SS[2]


# Exercise: Verify the eigen-value identity for Lambda statistic


# Example from Rencher 6.1.7 on p. 183
root <- read.table("T6_2_ROOT.DAT", 
                col.names = c('Tree.Number', 'Trunk.Girth.4.Years', 
                              'Ext.Growth.4.Years', 'Trunk.Girth.15.Years', 'Weight.Above.Ground.15.Years'))
root
# Exercise: What are the values of p, g, n?

# Note the groups are numbers and R will interpret them as such unless we convert to factor:
root$Tree.Number <- as.factor(root$Tree.Number)

# Create the list of the p dependent variables:
dependent.vars = cbind(root$Trunk.Girth.4.Years, root$Ext.Growth.4.Years, root$Trunk.Girth.15.Years, root$Weight.Above.Ground.15.Years)

summary(manova(dependent.vars ~ root$Tree.Number), test = "Wilks")
summary(manova(dependent.vars ~ root$Tree.Number), test = "Roy")
summary(manova(dependent.vars ~ root$Tree.Number))
summary(manova(dependent.vars ~ root$Tree.Number), test = "Hotelling-Lawley")

# Exercise: Calculate the Chi-square approximation