data{
    int<lower=0> J; 
    int y[J];     
    int N[J];          
}
parameters{
    vector<lower=0,upper=1>[J] theta;
    real<lower=0> alpha;
    real<lower=0> beta;    
}
model{
    y ~ binomial(N,theta); // target += binomal_lpdf(y |n, theta)
    theta ~ beta(alpha,beta); // target += beta_lpdf(theta | alpha, beta)
}



// data{
//     int<lower=0> J; 
//     int y[J];     
//     int N[J];          
// }
// parameters{
//     vector[J] theta;
//     real mu_theta;
//     real<lower=0> sigma_theta; 
// }
// transformed parameters{
//     vector[J] p;
//     p ~ inv_logit(theta)
// }
// model{
//     y ~ binomial(N,theta); // target += binomal_lpdf(y |n, theta)
//     theta ~ normal(mu_theta,sigma_theta); // target += beta_lpdf(theta | alpha, beta)
// }