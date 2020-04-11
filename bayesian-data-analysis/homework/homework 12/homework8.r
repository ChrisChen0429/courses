library(plyr)
# Linear map of points to a score between -1 and 1 
url_csv <- "http://www.football-data.co.uk/mmz4281/1516/E0.csv"
 # Data downloaded from football-data.co.uk
mydat <- read.csv(url(url_csv))
epl <- c()
# teams are assigned IDs 1, 2, ...:
epl$home_team <- as.numeric(mydat$HomeTeam)
epl$away_team <- as.numeric(mydat$AwayTeam)
epl$team_names <- levels(mydat$HomeTeam)
epl$home_goals <- mydat$FTHG # full time home goals
epl$away_goals <- mydat$FTAG # full time away goals
epl$score_diff <- epl$home_goals - epl$away_goals
# Points from last season are read and mapped to a score
epl$ngames <- length(epl$score_diff)
epl$nteams <- length(unique(epl$home_team))
epl$nweeks <- floor(2*epl$ngames /epl$nteams)
# The following code computes the week for each team in their games:
epl$home_week <- c()
epl$away_week <- c()
for (g in 1:epl$ngames) {
    epl$home_week[g] <- sum(epl$home_team[1:g] == epl$home_team[g]) + sum(epl$away_team[1:g] == epl$home_team[g])
    epl$away_week[g] <- sum(epl$away_team[1:g] == epl$away_team[g]) + sum(epl$home_team[1:g] == epl$away_team[g])
}
epl$bet_home <- mydat$B365H # Betting odds for home team win 
epl$bet_draw <- mydat$B365D # Betting odds for draw 
epl$bet_away <- mydat$B365A # Betting odds for away team win 

# epl$prev_perf you can use estimates from the Team Abilities After Week One 
epl$prev_perf <- c(0.5,1.2,0.01,0.6,-0.1,0,-0.12,-0.05,-0.13,0.45,0.55,-0,4,
            -0,56,0.01,-0.02,-1.5,-0.48,-0.6,-0.58,-0.54)

saveRDS(epl,'epl_data.rds')

## fitting the model
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
epl <- readRDS("epl_data.rds")
sm <- stan_model("epl_model.stan")
nsamples <- 1500
a_sims <- array(NA, c(nsamples, epl$nweeks, epl$nteams))
for (w in 1:38) {
    epl_w <- epl
    idx <- c(1:(w*10))
    epl_w$home_team <- epl$home_team[idx] 
    epl_w$away_team <- epl$away_team[idx] 
    epl_w$home_goals <- epl$home_goals[idx] 
    epl_w$away_goals <- epl$away_goals[idx] 
    epl_w$score_diff <- epl$score_diff[idx] 
    epl_w$home_week <- epl$home_week[idx]
    epl_w$away_week <- epl$away_week[idx]
    epl_w$ngames <- w*10
    epl_w$nweeks <- max(c(epl_w$home_week, epl_w$away_week))
    fit <- sampling(sm, chains = 4, iter = (nsamples/2), data = epl_w) 
    saveRDS(fit, paste("fit_", w, ".rds", sep=""))
    sims <- extract(fit)
    for (g in ((w-1)*10 + 1):(w*10)) {
        a_sims[,epl$home_week[g],epl$home_team[g]] <- sims$a[,epl$home_week[g],epl$home_team[g]]
        a_sims[,epl$away_week[g],epl$away_team[g]] <- sims$a[,epl$away_week[g],epl$away_team[g]]
    } 
}
saveRDS(a_sims,"a_sims.rds")