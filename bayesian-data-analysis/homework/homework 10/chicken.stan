data {
int J;
vector[J] y;
vector<lower=0>[J] sigma_y;
}
parameters {
vector[J] alpha;
real mu;
real<lower=0> tau;
}
transformed parameters{
  vector[J] theta;   
  // mu _ tau * alpha is the real treatment effect for different group
  theta = mu + tau*alpha;
}
model {
alpha ~ normal(0, 1);
y ~ normal(theta, sigma_y);  
  // mu is the average treatment effect
  // tau is the 38 true treatment effect standard deviation (how the treatment effects shift)
  // alpha[j] is the standarized treatment effect (whether this particular effect is high or low)
}