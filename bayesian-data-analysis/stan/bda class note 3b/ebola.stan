functions{
  int ebola_rng(int N, real alpha, real beta){
        real pi = beta_rng(alpha,beta);
        int y = binomial_rng(N,pi);
        return y;
    }
}
data{
    int<lower=1> exposed;
    int<lower=1,upper=exposed> survived;
    real<lower=1> alpha;
    real<lower=1> beta;
}
transformed data{
    int died = exposed - survived;
    // log of binomial coefficient
    real binomal_constant = lchoose(exposed,survived); 
    // negative log of beta coeffcient
    real beta_constant = -lbeta(alpha,beta);  
}
parameters{
    real<lower=0,upper=1> pi;
}
model{
    real log_pi = log(pi);
    //same as log(1 - pi) but better
    real log_1mpi = log1m(pi);
    // log likelihood: same as target += binomial_lpmf(survived / exposed,pi);
    target += binomal_constant + survived * log_pi + died * log_1mpi;
    // prior: same as target += beta_lpdf(pi / alpha,beta);
    target += beta_constant + (alpha -1) * log_pi + (beta - 1) * log_1mpi;
}
generated quantities{
    // theoritical true posterior
    real pi_post = beta_rng(alpha+survived,beta+died);
    real odds = pi / (1 - pi);
} 