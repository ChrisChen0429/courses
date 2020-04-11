# Density curves of various univariate normal distributions:

x <- seq(-5, 5, len= 1000)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.6))
lines(x, dnorm(x, m = -1), col = "red")
lines(x, dnorm(x, m = 1), col = "green")
lines(x, dnorm(x, sd = 1.5), col = "blue")
lines(x, dnorm(x, sd = 0.75), col = "gray54")

legend(-4.5, 0.5, legend=c("N(0,1)", "N(-1,1)", "N(1,0)", 
                           "N(0, 1.5^2)","N(0, 0.75^2)"), 
       col=c("black", "red", "green", "blue", "gray54"), lty = 1, cex=0.8)


# Density curves of various chi-square distributions:
x <- seq(0, 75, len= 500)
plot(x, dchisq(x, 4), type = "l", ylim = c(0, 0.2))
lines(x, dchisq(x, 10), col = "purple")
lines(x, dchisq(x, 20), col = "green")
lines(x, dchisq(x, 30), col = "blue")
lines(x, dchisq(x, 50), col = "gray54")

legend(50, 0.17, legend=c("df = 4", "df = 10", "df = 20", "df = 30", "df = 50"), 
       col=c("black", "purple", "green", "blue", "gray54"), lty = 1, cex=0.8)


# Density curves of various t distributions:

x <- seq(-5, 5, len= 200)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.5))
lines(x, dt(x, 3), col = "purple")
lines(x, dt(x, 11), col = "green")
lines(x, dt(x, 24), col = "blue")


legend(-4, 0.4, legend=c("N(0, 1)", "df = 3", "df = 11", "df = 24"), 
       col=c("black", "purple", "green", "blue"), lty = 1, cex=0.8)

# Density curves of various F distributions:

x <- seq(0, 10, len= 100)
plot(x, df(x, 10, 20), type = "l", ylim = c(0, 0.9))
lines(x, df(x, 5, 10), col = "purple")
lines(x, df(x, 5, 5), col = "yellow")


legend(6, 0.8, legend=c("df = (10, 20)", "df = (5, 10)", "df = (5, 5)"), 
       col=c("black", "purple", "yellow"), lty = 1, cex=0.8)


# Bivariate normal
# Create the grid first
x <- seq(-3, 3, len = 100)
y <- seq(-3, 3, len = 100)

# Standard bivariate normal
z1 <- (1/(2*pi))*exp(-outer(x^2, y^2, "+")/2)

# Save old paramaters
p = par()
# Make plot area square
par(pty = "s")
contour(x,y,z1)
image(x,y,z1)
persp(x, y, z1, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

# Or with package rgl
install.packages("rgl")
library("rgl")
persp3d(x,y,z1)

# Any bivariate normal (notice it is much slower because of the two loops)
x <- seq(-4, 4, len = 100)
y <- seq(-4, 4, len = 100)

#This is the covariance matrix. Make sure it has a positive determinant!
S <- matrix(c(11, 4, 4, 2), 2, 2)
z2 <- matrix(rep(0, 100^2), 100, 100)
d <- det(S)
for (i in 1:100)
  for (j in 1:100)
    z2[i, j] <- (1/(2*pi*sqrt(d)))*exp(-(t(c(x[i], y[j]))%*%solve(S)%*%c(x[i], y[j]))/2)

contour(x,y,z2)
persp(x,y,z2, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


# Simulating any multivariate normal data:

library(MASS)

# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
(Sigma <- matrix(c(1, .5, .5, 1), 2))  # Covariance matrix

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) 
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

# Example 4.10 on p. 180:
# Radiation through closed doors of 42 microwave ovens
d = read.table(file.choose())

par = p
qqp = qqnorm(d$V1)
qqline(d$V1)

cor(qqp$x, qqp$y)
# Not significant according to table on p. 181

# Hypotheses tests:
shapiro.test(d$V1)

# Transformations
y = log(d$V1)
qqnorm(y)
qqline(y)
shapiro.test(y)

# Box-Cox transformation with lambda = 0.25
x25 = (d$V1^0.25 - 1)/0.25

# Fig. 4.13 on p. 196:
qqnorm(x25)
qqline(x25)

shapiro.test(x25)

# Lambda = 0.25 made much bigger difference than log transform!