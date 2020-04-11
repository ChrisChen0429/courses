/* 
This is used to simulate the fake data.
*/

data{
  int<lower=2> nteams; // number of teams 
  int<lower=1> ngames; // number of games
  int<lower=1> nweeks; // number of weeks
  int<lower=1> home_week[ngames]; // week number for the home team
  int<lower=1> away_week[ngames]; // week number for the away team
  int<lower=1, upper=nteams> home_team[ngames]; // home team ID (1, ..., 20)
  int<lower=1, upper=nteams> away_team[ngames]; // away team ID (1, ..., 20)
  vector[nteams] prev_perf;// a score between -1 and 1 
}
model{
}
generated quantities{
  vector[ngames] score_diff; // home_goals - away_goals
  real<lower=0> nu;
  real<lower=0> tau_a;
  real b_prev;
  real<lower=0> sigma_a0;
  real<lower=0> sigma_y;
  vector[ngames] a_diff; 
  matrix[nweeks, nteams] a; // team abilities
  row_vector<lower=0>[nteams] sigma_a; // game-to-game variation
  row_vector<lower=0>[nteams] sigma_a_raw; // game-to-game variation
  matrix[nweeks,nteams] eta_a;         // random component
  real b_home; // the effect of hosting the game in mean of score_diff dist.

  // priors
  nu = fabs(gamma_rng(2,0.1)); 
  b_prev = normal_rng(0,1);
  sigma_a0 = fabs(normal_rng(0,1));
  sigma_y = fabs(normal_rng(0,5));
  b_home = normal_rng(0,1);
  for (i in 1:nteams){
    sigma_a_raw[i] = fabs(normal_rng(0,1));
  }
  tau_a = fabs(cauchy_rng(0,1));
  for (i in 1:nweeks){
    for (j in 1:nteams){
      eta_a[i,j] = normal_rng(0,1);
    }
  }
  
  // simulation the abilities matrix
  for (j in 1:nteams){
    a[1,j] = b_prev * prev_perf[j] + sigma_a0 * eta_a[1,j]; //initial abilities(at week 1)
  }
  for(i in 1:nteams){
      sigma_a[i] = fabs(tau_a * sigma_a_raw[i]);
  }
  for (w in 2:nweeks){
    for (j in 1:nteams){
    a[w,j] = a[(w-1),j] + sigma_a[j] * eta_a[w,j];  //evolution of abilities
    }
  }
  // simulate the game results
  for (g in 1:ngames){
    a_diff[g] = a[home_week[g],home_team[g]] - a[away_week[g],away_team[g]];
  }
  for (g in 1:ngames)
  score_diff[g] = round(student_t_rng(nu,a_diff[g] + b_home,sigma_y));
}

