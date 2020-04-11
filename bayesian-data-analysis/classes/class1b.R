library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

N <- 100
x <- runif(N, 0, 10)  #  seq(0, 10, length=N)
a <- 4
b <- 3
sigma <- .2
y_2018 <- a * sin(b*x) + rnorm(N, 0, sigma)

test <- list(N=N, x=x, y=y_2018)
fit <- stan("class1b.stan", data=test)
print(fit)

sims <- extract(fit)
n_sims <- length(sims$a)

plot(x, y_2018)
curve(a*sin(b*x), add=TRUE, col="red")
for (i in sample(n_sims,10)){
  a_post <- sims$a[i]
  b_post <- sims$b[i]
  curve(a_post*sin(b_post*x), add=TRUE, col="blue", lwd=.5)
}


