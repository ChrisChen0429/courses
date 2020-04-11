data{
  int<lower=0> N;
  vector[N] x;
  vector[N] y;

}
parameters{
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> sigma;
}
model{
  a ~ cauchy(2,1); 
  b ~ cauchy(3,1);
  y ~ normal(0,sigma ./ (a+b*x));  
}