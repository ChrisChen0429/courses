data{
    int<lower=0> J;
    int n[J];
    vector[J] x;
    int y[J];
}
parameters{
    real a;
    real b;
}
model{
    y ~ binomial_logit(n,a+b*x);
}