## linear item response model 
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


teams <- as.vector(unlist(read.table("soccerpowerindex.txt", header=FALSE)))
N_teams <- length(teams)
prior_score <- rev(1:N_teams)
prior_score <- (prior_score - mean(prior_score))/(2*sd(prior_score))
data2014 <- read.table ("worldcup2014.txt", header=FALSE)
N_games <- nrow(data2014)
team_1 <- match(as.vector(data2014[[1]]), teams)
score_1 <- as.vector(data2014[[2]])
team_2 <- match(as.vector(data2014[[3]]), teams)
score_2 <- as.vector(data2014[[4]])
df <- 7
data_worldcup <- c("N_teams","N_games","team_1","score_1","team_2","score_2","prior_score","df")
fit_1 <- stan("worldcup_first_try.stan", data=data_worldcup)
fit_1

## posterior plots
library(arm)
a_post <- extract(fit_1)$a
a_hat <- apply(a_post, 2, median)
a_se <- apply(a_post, 2, sd)
coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2), xlim=c(-.5,.5))

fit_2 <- stan("worldcup_with_replication.stan", data=data_worldcup)
sims <- extract(fit_2)$y_rep
q25 <- apply(sims, 2, quantile, 0.025)
q75 <- apply(sims, 2, quantile, 0.975)
coefplot (rev(score_1 - score_2), sds=rep(0, N_games),
          lower.conf.bounds=rev(q25), upper.conf.bounds=rev(q75),
          varnames=rev(paste(teams[team_1], "vs.", teams[team_2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
