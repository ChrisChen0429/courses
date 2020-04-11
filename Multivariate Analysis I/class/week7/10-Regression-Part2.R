# Example 10.4.2 on p. 357 of Rencher
# T10_1_CHEM.DAT
d <- read.table("T10_1_CHEM.DAT")
colnames(d) = c("Number", "Unchanged", "Converted", "Unwanted", "Temperature", "Concentration", "Time")
head(d)

# z variables
Z <- cbind(rep(1, nrow(d)), as.matrix(d[, 5:7]))
head(Z)

# y variables
Y <- as.matrix(d[, 2:4])
head(Y)

# Estimates of the coefficients
(B <- solve(t(Z) %*% Z) %*% t(Z) %*% Y)

# Fitted values:
Y.hat = Z %*% B
head(Y.hat)

# Residuals:
R = Y - Y.hat
head(R)

# Residual SS matrix:
(Sigma.hat = t(R) %*%R)

# Built-in function
mlm <- lm(cbind(Unchanged, Converted, Unwanted) ~ Temperature + Concentration + Time, data = d)
summary(mlm)
coef(mlm)

# Overall significance test

# Reduced model with only intercepts
Z1 <- cbind(rep(1, nrow(d)))
(B1 <- solve(t(Z1) %*% Z1) %*% t(Z1) %*% Y)

Y1.hat = Z1 %*% B1
R1 = Y - Y1.hat
(Sigma1.hat = t(R1) %*%R1)


# sigma.hat = E and Sigma1.hat = H
eigen(solve(Sigma.hat) %*% Sigma1.hat)
q = 3
p = 3
v_h = q
ve = nrow(d) - q - 1
s = min(q,p)
m = (1/2) *(abs(p-q)-1)
N = 1/2 * (nrow(d)-q-p-2)

# T.S
(det(Sigma.hat)/det(Sigma1.hat))^(nrow(d)/2)

# Likelihood ratio test:

n = nrow(d)
r = 3
m = 3
q = 0

# Chi-sq approx
-(n-r-1-(m-r+q+1)/2)*log(det(Sigma.hat)/det(Sigma1.hat))

# p-value
1-pchisq(-(n-r-1-(m-r+q+1)/2)*log(det(Sigma.hat)/det(Sigma1.hat)), m*(r-q))

# Conclusion: p-value < 0.05 we reject Ho


# Testing individual predictors:
anova(mlm)
summary(manova(mlm))
summary(manova(mlm), test="Wilks")
# Note anova and manova use sequential sum of square testing!

#install.packages("car")
library(car)
Manova(mlm)
# car package Manova uses Type II SS