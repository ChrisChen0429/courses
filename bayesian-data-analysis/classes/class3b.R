N <- 100
theta <- 5
sigma <- 5
y <- rnorm(N, theta, sigma)

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit <- stan("class3b.stan")
print(fit)
