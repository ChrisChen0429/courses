# create the fake data
a <- 5
b <- 0.3
sigma <- 0.5
N <- 100
x <- runif(N, 0, 10)
y <- rnorm(N, a*exp(-b*x), sigma)
curve(a*exp(-b*x), from=0, to=10, ylim=range(x, y), xlab="x", ylab="y", bty="l", main="Data and true model", cex.main=3)
points(x, y, pch=20, cex=1)

## fit the model
data <- list(N=N,
             x=x,
             y=y)
fit <- stan('exponential.stan',data = data)
print(fit)

a <- 5
b <- 0.3
sigma <- 0.5
N <- 100
x <- runif(N, 0, 10)
epsilon <- exp(rnorm(N, 0, sigma))
y <- a*exp(-b*x)*epsilon
curve(a*exp(-b*x), from=0, to=10, ylim=range(x, y), xlab="x", ylab="y", bty="l", main="Data and true model", cex.main=1)
points(x, y, pch=20, cex=0.2)
data <- list(N=N,x=x,y=y)
fit2 <- stan('exponential2.stan',data = data)
print(fit2)



a <- c(1, 0.8)
b <- c(0.1, 2)
N <- 1000
x <- seq(0, 10, length=N)
sigma <- 0.2
epsilon <- exp(rnorm(N, 0, sigma))
y <- (a[1]*exp(-b[1]*x) + a[2]*exp(-b[2]*x)) * epsilon

data_graph <- function(a, b, sigma, x, y) {
  curve(a[1]*exp(-b[1]*x) + a[2]*exp(-b[2]*x), from=min(x), to=max(x), ylim=c(0, 1.05*max(y)), xlim=c(0, max(x)),
        xlab="x", ylab="y", xaxs="i", yaxs="i", bty="l", main="Data and true model", cex.main=1)
  points(x, y, pch=20, cex=0.2)
  text(max(x), 0.5*max(y), paste("y = ", fround(a[1], 1), "*exp(", fround(-b[1], 1), "*x) + ", fround(a[2], 1), "*exp(", fround(-b[2], 1), "*x)", sep=""), adj=1)
}
data_graph(a, b, sigma, x, y)
data_3 <- list(N=N, x=x, y=y)
fit_3 <- stan("sumexpoential.stan", data=data_3)
fit_3
