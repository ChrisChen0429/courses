# the model is following:
# logit(y_j/n_j) = a + b * x, where p_j = y_j / n_j
# y ~ binomial(n_j,p_j)


# logistic regression
y <- c(1346,577,337,208,149,136,111,69,67,75,52,46,54,28,27,31,33,20,24)
n <- c(1443,674,455,353,272,256,240,217,200,237,202,192,174,167,201,193,191,147,152)
x <- 1:19
J <- 19
data <- list(y=y,x=x,n=n,J=J)
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)
result <- stan('golf.stan',data=data) 
print(result,digit=2)

p_hat <- y / n
fit <- extract(result)
a_mean <- mean(fit$a)
b_mean <- mean(fit$b) 
n_of_simulation <- length(fit$a)
plot(x,p_hat)
 curve(exp(a_mean + b_mean*x)/(1+exp(a_mean + b_mean*x)),add=T,col='red',lwd=1)
for ( i in sample(n_of_simulation,20)){
    a_post <- fit$a[i]
    b_post <- fit$b[i]
   curve(exp(a_post + b_post*x)/(1+exp(a_post + b_post*x)),add=T,col='blue',lwd=0.5)
}

# model from first principle
data <- list(y=y,x=x,n=n,J=J,r=1.68*0.0833333,R=4.25*0.0833333)  ### inch to feet
result_2 <- stan('golf_first.stan',data=data)
print(result_2,digit=2)
fit_2 <- extract(result_2)
sigma_mean <- mean(fit_2$sigma)
plot(x,p_hat)
curve(exp(a_mean + b_mean*x)/(1+exp(a_mean + b_mean*x)),add=T,col='red',lwd=1)
curve(2*pnorm(asin((data$R-data$r)/x)/sigma_mean)-1, add=T,col='blue',lwd=1)
