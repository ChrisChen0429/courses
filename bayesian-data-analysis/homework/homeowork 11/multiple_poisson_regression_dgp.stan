functions {
/*
   * Alternative to poisson_log_rng() that 
   * avoids potential numerical problems during warmup
*/
   int poisson_log_safe_rng(real eta) {
     real pois_rate = exp(eta);
     if (pois_rate >= exp(20.79))
       return -9;
     return poisson_rng(pois_rate);
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
   for (n in 1:N) {
     traps[n] = poisson_rng(mean_traps);
     live_in_super[n] = bernoulli_rng(mean_live_in_super);
     log_sq_foot[n] = normal_rng(mean_log_sq_foot,1);
     complaints[n] = poisson_log_safe_rng(alpha + beta * traps[n] + beta_super * live_in_super[n] + log_sq_foot[n]);}
}
