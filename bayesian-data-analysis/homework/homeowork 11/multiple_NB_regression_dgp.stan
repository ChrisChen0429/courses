functions {
   /*
   * Alternative to neg_binomial_2_log_rng() that 
   * avoids potential numerical problems during warmup
   */
   int neg_binomial_2_log_safe_rng(real eta, real phi) {
     real gamma_rate = gamma_rng(phi, phi / exp(eta));
     if (gamma_rate >= exp(20.79))
       return -9;     
     return poisson_rng(gamma_rate);
   }
}
data{
    int<lower=1> N;
    real<lower=0> mean_traps;
    real<lower=0> mean_live_in_super;
    real<lower=1> mean_log_sq_foot;
}
model{
}
generated quantities {
   int traps[N];
   int complaints[N];
   int live_in_super[N];
   real<lower=1> log_sq_foot[N]; 
   real beta = normal_rng(-0.25, 1);
   real beta_super = normal_rng(-0.5, 1);
   real alpha = normal_rng(log(4), 1); 
   real<lower=0> inv_phi = normal_rng(1, 1); 
   for (n in 1:N) {
     traps[n] = poisson_rng(mean_traps);
     live_in_super[n] = bernoulli_rng(mean_live_in_super);
     log_sq_foot[n] = normal_rng(mean_log_sq_foot,1);
     complaints[n] = neg_binomial_2_log_safe_rng(alpha + beta * traps[n] + beta_super * live_in_super[n] + log_sq_foot[n], inv(inv_phi));}
}






