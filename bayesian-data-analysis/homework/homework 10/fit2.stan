data{
  int<lower=0> N;
  real y[N];
  real<lower=0> sigma[N];
}
parameters{
  real mu;
  real<lower=0> tau;
  real eta[N];
}
transformed parameters{
  real theta[N];
  for (i in 1:N){
    theta[i] = mu + tau * eta[i];
  }
}
model{
  target += normal_lpdf(eta |0,1);
  target += normal_lpdf(y | theta,sigma);
}