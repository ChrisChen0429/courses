library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

N <- 100
x <- runif(N, 0, 10)  #  seq(0, 10, length=N)
a <- 2
b <- 3
sigma <- 0.2
y <- 1/(a + b*x) + rnorm(N, 0, sigma)

fake <- list(N=N, x=x, y=y)
fit <- stan("hwk3a.stan", data=fake)
print(fit)

sims <- extract(fit)
n_sims <- length(sims$a)

plot(x, y)
curve(1/(a+b*x) , add=TRUE, col="red")
for (i in sample(n_sims,10)){
  a_post <- sims$a[i]
  b_post <- sims$b[i]
  curve(1/(a_post + b_post*x), add=TRUE, col="blue", lwd=.5)
}


