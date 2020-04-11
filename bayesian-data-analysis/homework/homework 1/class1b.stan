data {
  int N;
  vector[N] y;
  vector[N] x;
}
parameters {
  real a;
  real<lower=0> b;
  real<lower=0> sigma;
}
model {
  y ~ normal(a * sin(b*x), sigma);
  a ~ normal(0, 10);
  b ~ normal(0, 10);
  sigma ~ normal(0, 10);
}
