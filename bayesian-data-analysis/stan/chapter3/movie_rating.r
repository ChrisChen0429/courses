# Notation:
# y_i,i = 1,...,N=102
# j[i], where j[1] = j[2] = 1 and j[3] = ... = j[102] = 2
# y_i ~ normal(\theta_j[i],sigma)
# rating is within [0,5]


## case one: two movies
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)
# data 
y_1 <- c(3,5)
y_2 <- rep(c(2,3,4,5),c(10,20,30,40))
y <- c(y_1,y_2)
N <- length(y)
J <- rep(c(1,2),c(length(y_1),length(y_2)))
data <- list(y=y,N=N,J=J)
fit_1 <- stan('movie_rating.stan',data=data)
print(fit_1)


## case two: many movies
## theta_j ~ normal(3,0.5) for j = 1,...J
## z_i ~ normal(theta_j[i],2) for i = 1,...n
## j[i] indecate which movie rater i has been viewed, the value can be 1,...,J
## assume the J = 40 n = 880

true_theta <- rnorm(n=40,mean=3,sd=0.5)
n_j <- rep(c(20,30,20,30,10),8)
J <- length(n_j)
movie <- rep(1:40,n_j)
y <- c()
for (j in 1:40){
    y_this <- rnorm(n=n_j[j],mean=true_theta[j],sd=2)
    y <- c(y,y_this)
}
for (i in 1:length(y)){
    if (y[i] < 0){
        y[i] = 0
    }
    if (y[i] > 5){
        y[i] = 5
    }
}
N <- length(y)
data_2 <- list(N=N,y=y,movie=movie,J=J)
fit_2 <- stan('movie_rating_manymovies.stan',data=data_2)
print(fit_2)
result <- extract(fit_2)
median <- c()
p_0.025 <- c()
p_0.25 <- c()
p_0.75 <- c()
p_0.975 <- c()
col_all <- c()
for (i in 1:40){
    this <- quantile(result$theta[,i],c(0.025,0.25,0.5,0.75,0.975))
    if (true_theta[i] > this[1] & true_theta[i] < this[4]){
        col = 'blue'
    }else{
        col = 'red'
    }
    median <- c(median,this[3])
    p_0.025 <- c(p_0.025,this[1])
    p_0.25 <- c(p_0.25,this[2])
    p_0.75 <- c(p_0.75,this[4])
    p_0.975 <- c(p_0.975,this[5])
    col_all <- c(col_all,col)
}
plot(true_theta,median,ylab='posterior meandian,50% and 95% interval',xlab='true parameters',xlim=c(0,5),ylim=c(0,5))
segments(x0=true_theta,y0=p_0.25,x1=true_theta,y1=p_0.75,col=col,lwd=3)
segments(x0=true_theta,y0=p_0.025,x1=true_theta,y1=p_0.975,col=col,lwd=1)
segments(0,0,5,5,lwd=1)


## case three: item-response model 
# y_i ~ normal(mu+sigma_a*alpha_j[i]-sigma_b*beta__k[i],sigma_y)
# alpha_j ~ normal(0,1) for j = 1,....,J
# beta_k ~ normal(0,1), for k = 1,....,K
# here let the J = 40 K = 100;
# true values:
true_mu <- 3
true_sigma_a <- 0.5
true_sigma_b <- 0.5
ture_sigma_y <- 2
true_alpha <- rnorm(n=40)
true_beta <- rnorm(n=100)
y <- c()
for (i in 1:40){
    for (j in 1:100){
        this <- rnorm(1,true_mu + true_sigma_a * true_alpha[i] - true_sigma_b * true_beta[j],ture_sigma_y)
        y <- c(y,this)
    }
}
movie <- rep(1:40,each=100)
rater <- rep(1:100,40)
data <- list(N=length(y),y=y,J=400,K=100,movie=movie,rater=rater)
fit_3 <- stan('movie_rating_itemresponse.stan',data=data)
print(fit_3)
## for alpha
result <- extract(fit_3)
median <- c()
p_0.025 <- c()
p_0.25 <- c()
p_0.75 <- c()
p_0.975 <- c()
col_all <- c()
for (i in 1:40){
    this <- quantile(result$alpha[,i],c(0.025,0.25,0.5,0.75,0.975))
    if (true_alpha[i] > this[1] & true_alpha[i] < this[4]){
        col = 'blue'
    }else{
        col = 'red'
    }
    median <- c(median,this[3])
    p_0.025 <- c(p_0.025,this[1])
    p_0.25 <- c(p_0.25,this[2])
    p_0.75 <- c(p_0.75,this[4])
    p_0.975 <- c(p_0.975,this[5])
    col_all <- c(col_all,col)
}
plot(true_alpha,median,ylab='posterior meandian,50% and 95% interval',xlab='true parameters',xlim=c(-5,5),ylim=c(-5,5))
segments(x0=true_alpha,y0=p_0.25,x1=true_alpha,y1=p_0.75,col=col,lwd=3)
segments(x0=true_alpha,y0=p_0.025,x1=true_alpha,y1=p_0.975,col=col,lwd=1)
segments(-5,-5,5,5,lwd=1)

## for beta
result <- extract(fit_3)
median <- c()
p_0.025 <- c()
p_0.25 <- c()
p_0.75 <- c()
p_0.975 <- c()
col_all <- c()
for (i in 1:100){
    this <- quantile(result$beta[,i],c(0.025,0.25,0.5,0.75,0.975))
    if (true_beta[i] > this[1] & true_beta[i] < this[4]){
        col = 'blue'
    }else{
        col = 'red'
    }
    median <- c(median,this[3])
    p_0.025 <- c(p_0.025,this[1])
    p_0.25 <- c(p_0.25,this[2])
    p_0.75 <- c(p_0.75,this[4])
    p_0.975 <- c(p_0.975,this[5])
    col_all <- c(col_all,col)
}
plot(true_beta,median,ylab='posterior meandian,50% and 95% interval',xlab='true parameters',xlim=c(-5,5),ylim=c(-5,5))
segments(x0=true_beta,y0=p_0.25,x1=true_beta,y1=p_0.75,col=col,lwd=3)
segments(x0=true_beta,y0=p_0.025,x1=true_beta,y1=p_0.975,col=col,lwd=1)
segments(-5,-5,5,5,lwd=1)