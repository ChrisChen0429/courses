data{
    int<lower=0> N;
    vector[N] y;
    vector[N] x;
}
parameters{
    real beta;
    real alpha;
    real<lower=0> sigma;
}
model{
    beta ~ cauchy(1,1);
    y ~ normal(alpha + beta * x,sigma);
}