library("foreign")
pew <- read.dta("pew_research_center_june_elect_wknd_data.dta")
state_num <- as.numeric(pew$state)
## y <- ifelse(pew$ideo %in% c("very liberal", "liberal"), 1, 0)
y <- ifelse(pew$ideo == "very liberal", 1, 0)
keep <- ! state_num %in% c(2, 9, 12)  & !is.na(state_num) & !is.na(y)  # exclude respondents from AK, HI, DC and cases with missing data
y <- y[keep]
state_num <- state_num[keep]
y_state <- rep(NA, 51)
n_state <- rep(NA, 51)
for (s in 1:51){
  y_state[s] <- sum(y[state_num==s])
  n_state[s] <- sum(state_num==s)
  if (n_state[s] == 0) y_state[s] <- 0
}

election <- read.csv("2008ElectionResult.csv")
dvote08 <- election[,"vote_Obama"]/(election[,"vote_Obama"] + election[,"vote_McCain"])
state.abbs <- c(state.abb[1:8], "DC", state.abb[9:50])

pew_data <- list(y=y_state, n=n_state, J=51, u=dvote08)

library("rstan")
options(mc.cores = parallel::detectCores())

fit_1 <- stan("exercise2.21_1.stan", data=pew_data)
print(fit_1)

fit_1_gamma <- stan("exercise2.21_1_gamma.stan", data=pew_data)
print(fit_1_gamma)

fit_2 <- stan("exercise2.21_2.stan", data=pew_data)
print(fit_2)

fit_3 <- stan("exercise2.21_3.stan", data=pew_data)
print(fit_3)

state_graph_raw <- function(condition=rep(TRUE, 51)){
  plot(x_range, y_range, bty="l", type="n", xlab="Obama vote share in 2008", ylab="Sample proportion very liberal", main="Raw data")
  text(dvote08[not_ak_hi_dc & condition], ybar_state[not_ak_hi_dc & condition], state.abbs[not_ak_hi_dc & condition])
}

state_graph_bayes <- function(fit, model_name, condition=rep(TRUE, 51), style="median"){
  sims <- extract(fit)
  if (style=="median"){
    p_display <- apply(sims$p, 2, median)
  }
  else if (style=="1draw"){
    n_sims <- 4000 # sorry!
    p_display <- sims$p[sample(n_sims,1),]
  }
  plot(x_range, y_range, bty="l", type="n", xlab="Obama vote share in 2008", ylab="Estimated proportion very liberal", main=model_name)
  text(dvote08[not_ak_hi_dc & condition], p_display[not_ak_hi_dc & condition], state.abbs[not_ak_hi_dc & condition])
}

sample_size_graph <- function(fit, model_name){
  sims <- extract(fit)
  p_median <- apply(sims$p, 2, median)
  plot(range(n_state), c(0, 0.1), bty="l", type="n", xlab="Sample size", ylab="Raw (blue) and estimated (red)", main=model_name)
  text(n_state[not_ak_hi_dc], ybar_state[not_ak_hi_dc], state.abbs[not_ak_hi_dc], col="blue", cex=.8)
  text(n_state[not_ak_hi_dc], p_median[not_ak_hi_dc], state.abbs[not_ak_hi_dc], col="red", cex=.8)
}

## Plot the estimates

par(mfrow=c(2,2))
par(mar=c(3,3,1,0), tck=-.01, mgp=c(1.5,.5,0))

not_ak_hi_dc <- ! state.abbs %in% c("AK", "HI", "DC")
ybar_state <- y_state/n_state
x_range <- range(dvote08[not_ak_hi_dc])
y_range <- range(ybar_state[not_ak_hi_dc])

## state_graph_raw()
state_graph_bayes(fit_2, "Lognormal model")
## sample_size_graph(fit_2, "Lognormal model")
## big_state <- n_state > 1000
## state_graph_raw(condition=big_state)
## state_graph_bayes(fit_1, "Beta model", condition=big_state)
state_graph_bayes(fit_3, "Lognormal with predictor")

## state_graph_bayes(fit_1_gamma, "Gamma model")
## state_graph_bayes(fit_2, "Lognormal model")

## Plot 1 simulation draw

state_graph_bayes(fit_2, "Lognormal model, 1 draw", style="1draw")
## big_state <- n_state > 1000
## state_graph_raw(condition=big_state)
## state_graph_bayes(fit_1, "Beta model", condition=big_state)
state_graph_bayes(fit_3, "Lognormal with predictor, 1 draw", style="1draw")

## state_graph_bayes(fit_1_gamma, "Gamma model")
## state_graph_bayes(fit_2, "Lognormal model")


