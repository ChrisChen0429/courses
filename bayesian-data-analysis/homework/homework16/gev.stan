functions {
  real gev_lpdf(vector y, real mu, real xi, real sigma) {
    // generalised gev log pdf 
    int N = rows(y);
    real inv_xi = inv(xi);
    real inv_sigma = inv(sigma);
    vector[N] lpdf_y;
    
    if (xi > 0 && min(y) < mu-sigma/xi){
      reject("xi > 0 && min(y) < mu-sigma/xi, found mu, xi, sigma = ", mu, xi, sigma)}
    if (xi < 0 && max(y) > mu-sigma/xi){
      reject("xi < 0 && max(y) > mu-sigma/xi, found mu, xi, sigma = ", mu, xi, sigma)}
    if (sigma<=0){
      reject("sigma<=0; found sigma =", sigma)}
      
    if (fabs(mu) > 1e-15){
      for (i in 1:N){
        lpdf_y[i] = (xi+1) * (-inv_xi) * log( 1+xi*((y[i]-mu)*inv_sigma)) -  log(sigma) -  (1+xi*((y[i]-mu)*inv_sigma)) ^ -inv_xi ;
      }
      return(sum(lpdf_y));}
    else{
      for (i in 1:N){
        lpdf_y[i] = (xi+1)* inv_sigma * (y[i]-mu) -  log(sigma) - exp((y[i]-mu)*(-inv_sigma));  
      }
      return(sum(lpdf_y));}
  }
  
  real gev_rng(real mu, real xi, real sigma) {
    // generalised Pareto rng
    real inv_xi = inv(xi);
    real inv_sigma = inv(sigma);
    
    if (sigma<=0){
      reject("sigma<=0; found sigma =", sigma)}
      
    if (fabs(mu) > 1e-15){
         return( mu + sigma / xi * ((-log(uniform_rng(0,1)))^(-xi) - 1));}
    else{
      return( mu - log(-sigma * log(uniform_rng(0,1))) );}
}}

data {
  real xi;
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real<lower=0> sigma; 
  real mu; 
}
model {
  y ~ gev(xi, mu, sigma);
}
generated quantities {
  vector[N] yrep;
  for (n in 1:N) {
    yrep[n] = gev_rng(xi, mu, sigma);
  }
}