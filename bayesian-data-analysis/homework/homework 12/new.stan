/* 
This is model is the new model 
*/

data{
  int<lower=2> nteams; // number of teams 
  int<lower=1> ngames; // number of games
  int<lower=1> nweeks; // number of weeks
  int<lower=1> home_week[ngames]; // week number for the home team
  int<lower=1> away_week[ngames]; // week number for the away team
  int<lower=1, upper=nteams> home_team[ngames]; // home team ID (1, ..., 20)
  int<lower=1, upper=nteams> away_team[ngames]; // away team ID (1, ..., 20)
  vector[ngames] score_diff;    // home_goals - away_goals
  vector[nteams] prev_perf;// a score between -1 and 1 
}
transformed data{
  vector[ngames] sqrt_dif;
  for (i in 1:ngames){
        sqrt_dif[i] = 2*(step(score_diff[i]) - 0.5)*sqrt(fabs(score_diff[i]));
  }
}
parameters{
  real b_home; // the effect of hosting the game in mean of score_diff dist.
  real b_prev;
  real<lower=0> sigma_a0;
  real<lower=0> tau_a;
  real<lower=0> sigma_y;
  row_vector<lower=0>[nteams] sigma_a_raw; // game-to-game variation
  matrix[nweeks,nteams] eta_a;         // random component
}
transformed parameters{
  matrix[nweeks, nteams] a; // team abilities
  row_vector<lower=0>[nteams] sigma_a; // game-to-game variation
  // simulation the abilities matrix
  for (j in 1:nteams){
    a[1,j] = b_prev * prev_perf[j] + sigma_a0 * eta_a[1,j]; //initial abilities(at week 1)
  }
  for(i in 1:nteams){
    sigma_a[i] = fabs(tau_a * sigma_a_raw[i]);
  }
  for (w in 2:nweeks){
    a[w,] = a[w-1,] + sigma_a .* eta_a[w,];
  }
}
model{
  vector[ngames] a_diff; 
  // priors
  b_prev ~ cauchy(0,1);
  sigma_a0 ~ cauchy(0,1);
  sigma_y ~ cauchy(0,1);
  b_home ~ cauchy(0,1);
  sigma_a_raw ~ cauchy(0,1);
  tau_a ~ cauchy(0,1);
  to_vector(eta_a) ~ cauchy(0,1);
  for (g in 1:ngames){
    a_diff[g] = a[home_week[g],home_team[g]] - a[away_week[g],away_team[g]];
  }
  for (g in 1:ngames){
    sqrt_dif ~ student_t(7,a_diff + b_home,sigma_y);
  }
}
generated quantities{
  vector[ngames] score_diff_rep;
  for (g in 1:ngames){
        score_diff_rep[g] = student_t_rng(7, a[home_week[g],home_team[g]] - a[away_week[g],away_team[g]]+b_home, sigma_y);
  }
}
