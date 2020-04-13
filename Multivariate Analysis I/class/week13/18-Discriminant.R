# Simulated data example:
library(MASS)

# Sample sizes
n1 = 400
n2 = 400

# Group 1
x1 <- mvrnorm(n1, c(5, 7), matrix(c(14,7,7,5),2, 2))
plot(x1, ylim = c(-5, 15), asp = 1, cex = 0.5)

# Group 2
x2 <- mvrnorm(n2, c(4, 3), matrix(c(14,7,7,5),2, 2))
points(x2, col = "red", cex = 0.5)

# Sample averages:
x1.bar <- apply(x1, 2, mean)
x2.bar <- apply(x2, 2, mean)
points(x1.bar[1], x1.bar[2], cex = 1.5, pch=19)
points(x2.bar[1], x2.bar[2], cex = 1.5, pch=19, col = "red")

# Sample covariance matrices:
S1 <- cov(x1)
S2 <- cov(x2)
Sp <- (1/(n1 + n2 -2))*((n1-1)*S1 + (n2-1)*S2)

(a <- solve(Sp)%*%(x1.bar - x2.bar))

# Line on which the projections are separated the best
abline(0,a[2]/a[1], col = "blue")


# Illustrating the separation:
# Combined sample
hist(x1, col=rgb(1,0,0,0.5), main = "Combined samples")
hist(x2, col=rgb(0,0,1,0.5), add=T)

# Combined sample projected on the best separating line: 
hist(x1 %*% a, col=rgb(1,0,0,0.5), xlim = c(-5, 20), main = "Combined separated samples")
hist(x2 %*% a, col=rgb(0,0,1,0.5), add=T)

# Another view:
plot((x1[,1]))
points(x2[,1], col = "red")

plot((x1%*%a), ylim = c(-5, 20))
points(x2%*%a, col = "red")


## Example 11.1 on p. 578
# Read data from Table 11.:
mower = read.table(file.choose())
colnames(mower) = c("Income", "LotSize", "Group")

# Scatterplot by group:
library(car)
scatterplot(LotSize ~ Income | Group, data = mower, regLine = FALSE, smooth = FALSE)

library(MASS)
ldamod <- lda(Group ~ ., data= mower, prior=rep(1/2, 2))
# check the LDA coefficients/scalings
ldamod

# Plot the group means:
m1 = ldamod$means[1,]
m2 = ldamod$means[2,]

points(m1[1], m1[2], pch = 19)
points(m2[1], m2[2], pch = 19)

abline(0, ldamod$scaling[1]/ldamod$scaling[2], col = 2)

predict(ldamod)
table(Predicted=predict(ldamod)$class, Actual=mower$Group)


## Bank example
# Read data by copying from Excel on a Mac:
my_data = read.table(pipe("pbpaste"), sep="\t", header = TRUE)

(ldamod2 = lda(popn ~ ., data= my_data))

plot(ldamod2)
table(Predicted=predict(ldamod2)$class, Actual= my_data$popn)
# Misclassificaion error = 4/46 = 0.087 !

# Accuracy:
mean(predict(ldamod2)$class==my_data$popn)

# Quadratic model:
qda.fit <- qda(popn ~ ., data= my_data)
qda.fit
qda.class <- predict(qda.fit)$class
table(qda.class, my_data$popn)
# Worse than linear :(