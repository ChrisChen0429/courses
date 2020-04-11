data{
  int<lower=0> N;
  real y_s[N];
  real<lower=0> sigma_s[N];
  real y_e[N];
  real<lower=0> sigma_e[N];

}
parameters{
  real mu_s;
  real<lower=0> tau_s;
  real eta_s[N];
  real mu_e;
  real<lower=0> tau_e;
  real eta_e[N];

}
transformed parameters{
  vector[N] theta_s;
  vector[N] theta_e;
  vector[N] ratios;
  for (i in 1:N){
    theta_s[i] = mu_s + tau_s * eta_s[i];
    theta_e[i] = mu_e + tau_e * eta_e[i];
    ratios[i] = theta_e[i] / theta_s[i];
  }
}
model{
  target += normal_lpdf(eta_s |0,1);
  target += normal_lpdf(y_s | theta_s,sigma_s);
  target += normal_lpdf(eta_e |0,1);
  target += normal_lpdf(y_e | theta_e,sigma_e);
}
generated quantities{
  real overall_ratio;
  overall_ratio = mu_e / mu_s;
}