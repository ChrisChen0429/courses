## here is an example of using rstan to solve a ebola bayesian problem
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)

## prior information: based on the experiment result from monkey  
mode <- 2/3
median <- 0.55
N <- 7L
prior_alpha <- (median*(4*mode-3) + mode) / (3 * (mode - median))
prior_beta <- (median*(1- 4*mode) + 5*mode -2) / (3 * (mode - median))

## prediction
expose_stan_functions('ebola.stan')
args(ebola_rng)
ebola_rng(N,prior_alpha,prior_beta) # the one prediction of survie is 0

# lookup for the r and stan distribution transformation
lookup('rbeta')

# run the result and compare the theorical and simulated posterior
# plug in the real experiment data for human
data <- list(
    exposed = 7,
    survived = 5,
    alpha = prior_alpha,
    beta = prior_beta
)
result <- stan('ebola.stan',refresh=0,data=data)
print(result,digit=2)
