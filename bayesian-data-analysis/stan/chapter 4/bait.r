pest_data <- readRDS('pest_data.RDS')
str(pest_data)

N_buildings <- length(unique(pest_data$building_id))
hist(pest_data$complaints,breaks=25)

#  possion regression
## generate the fake data
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#### For sampling the fake data there is no parameters in the model or even the model block, can only use the fixed parameter algorithum
#### Here we set the iter =1 since we only need one set of fake parameters otherwise half of the iteration results would be kept
#### the combination of stan_model and sampling is the same as stan()
comp_dgp_simple <- stan_model('simple_possion_regression_dgp.stan')
fitted_model_dgp <- sampling(comp_dgp_simple,
                        data = list(N=nrow(pest_data),mean_traps = mean(pest_data$traps)),
                        chains=1,iter=1,algorithm='Fixed_param',seed=123)
sims_dgp <- extract(fitted_model_dgp) 
str(sims_dgp)

## fitting the model to the fake data
#### without specifiation, the iter number in the sampling() or stan() is 8000 and the half of simulation would be kept
stan_dat_fake <- list(N= nrow(pest_data),
                      traps = sims_dgp$traps[1,],
                      complaints = sims_dgp$complaints[1,])
str(stan_dat_fake)
comp_model_P <- stan_model('bait_possion.stan')
fit_model_P <- sampling(comp_model_P, data = stan_dat_fake, seed = 123)

## assessing parameter recovery
posterior_alpha_beta <- as.matrix(fit_model_P, pars = c('alpha','beta'))
head(posterior_alpha_beta)
true_alpha_beta <- c(sims_dgp$alpha, sims_dgp$beta)
library(bayesplot)
mcmc_recover_hist(posterior_alpha_beta, true = true_alpha_beta)

y_rep <- as.matrix(fit_model_P,pars = 'y_rep')
ppc_dens_overlay(y = stan_dat_fake$complaints, yrep = y_rep[1:2000, ])
ppc_rootogram(stan_dat_fake$complaints, yrep = y_rep)

## fiting with the data supplied to us
stan_dat_simple <- list(N = nrow(pest_data), complaints = pest_data$complaints, traps = pest_data$traps)
fit_P_real_data <- sampling(comp_model_P, data = stan_dat_simple)
print(fit_P_real_data, pars = c('alpha','beta'))
y_rep <- as.matrix(fit_P_real_data, pars = "y_rep")
ppc_dens_overlay(y = stan_dat_simple$complaints, y_rep[1:200,])

prop_zero <- function(x) mean(x == 0)
ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero")


mean_y_rep <- colMeans(y_rep)
std_resid <- (stan_dat_simple$complaints - mean_y_rep) / sqrt(mean_y_rep)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

ppc_rootogram(stan_dat_simple$complaints, yrep = y_rep)

ppc_intervals(y = stan_dat_simple$complaints, yrep = y_rep, x = stan_dat_simple$traps) + 
  labs(x = "Number of bait stations", y = "Number of complaints")






## expanding the model: multiple predictiors
ggplot(pest_data, aes(x = log(total_sq_foot), y = log1p(complaints))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

stan_dat_simple$log_sq_foot <- log(pest_data$total_sq_foot/1e4)
stan_dat_simple$live_in_super <- pest_data$live_in_super

## simulate fake data with multiple predictors
comp_dgp_multiple <- stan_model('multiple_poisson_regression_dgp.stan')
stan_dat_fake <- list(N=nrow(pest_data),
                      mean_traps = mean(pest_data$traps),
                      mean_live_in_super =mean(pest_data$live_in_super),
                      mean_log_sq_foot =  mean(log(pest_data$total_sq_foot)))
fitted_model_dgp <- sampling(comp_dgp_multiple, data = stan_dat_fake,chains = 1, cores = 1, iter = 1, algorithm = 'Fixed_param', seed = 123)
sims_dgp <- extract(fitted_model_dgp)
stan_dat_fake <- list(N = nrow(pest_data), log_sq_foot = sims_dgp$log_sq_foot[1, ],
  live_in_super = sims_dgp$live_in_super[1, ], traps = sims_dgp$traps[1, ], complaints = sims_dgp$complaints[1, ])

comp_model_P_mult <- stan_model('multiple_poisson_regression.stan')
fit_model_P_mult <- sampling(comp_model_P_mult, data = stan_dat_fake, chains = 4, cores = 4)

posterior_alpha_beta <- as.matrix(fit_model_P_mult, pars = c('alpha','beta','beta_super'))
true_alpha_beta <- c(sims_dgp$alpha,sims_dgp$beta,sims_dgp$beta_super)
mcmc_recover_hist(posterior_alpha_beta, true = true_alpha_beta)

## Fit the data given to us
fit_model_P_mult_real <- sampling(comp_model_P_mult, data = stan_dat_simple)
y_rep <- as.matrix(fit_model_P_mult_real, pars = "y_rep")
ppc_dens_overlay(stan_dat_simple$complaints, y_rep[1:200,])

prop_zero <- function(x) mean(x == 0)
ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero", binwidth = 0.01)

ppc_intervals(y = stan_dat_simple$complaints, yrep = y_rep, x = stan_dat_simple$traps) + 
  labs(x = "Number of bait stations", y = "Number of complaints")












## Modeling count data with the negative binomial distribution
comp_dgp_multiple_NB <- stan_model('multiple_NB_regression_dgp.stan')
stan_dat_fake <- list(N=nrow(pest_data),
                      mean_traps = mean(pest_data$traps),
                      mean_live_in_super =mean(pest_data$live_in_super),
                      mean_log_sq_foot =  mean(log(pest_data$total_sq_foot)))
fitted_model_dgp_NB <- sampling(comp_dgp_multiple_NB, data = stan_dat_fake,
  chains = 1, cores = 1, iter = 1, algorithm = 'Fixed_param', seed = 123)
sims_dgp_NB <- extract(fitted_model_dgp_NB)

stan_dat_fake_NB <- list(N = nrow(pest_data), 
                         log_sq_foot = sims_dgp_NB$log_sq_foot[1, ],
                         live_in_super = sims_dgp_NB$live_in_super[1, ],
                         traps = sims_dgp_NB$traps[1, ],
                         complaints = sims_dgp_NB$complaints[1, ]
)

comp_model_NB <- stan_model('multiple_NB_regression.stan')

fitted_model_NB <- sampling(comp_model_NB, data = stan_dat_fake_NB, 
                            chains = 4, cores = 4)
 
posterior_alpha_beta_NB <- as.matrix(fitted_model_NB, pars = c('alpha', 'beta', 'beta_super', 'inv_phi'))
true_alpha_beta_NB <- c(sims_dgp_NB$alpha, sims_dgp_NB$beta, sims_dgp_NB$beta_super, sims_dgp_NB$inv_phi)
mcmc_recover_hist(posterior_alpha_beta_NB, true = true_alpha_beta_NB)

## fiting to the given data and checking the fit
fitted_model_NB <- sampling(comp_model_NB, data = stan_dat_simple)
sims_NB <- extract(fitted_model_NB)

y_rep <- sims_NB$y_rep
ppc_dens_overlay(stan_dat_simple$complaints, y_rep[1:200,])

ppc_stat(y = stan_dat_simple$complaints, yrep = y_rep, stat = "prop_zero")

mean_inv_phi <- mean(sims_NB$inv_phi)
mean_y_rep <- colMeans(y_rep)
std_resid <- (stan_dat_simple$complaints - mean_y_rep) / sqrt(mean_y_rep + mean_y_rep^2*mean_inv_phi)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

ppc_rootogram(stan_dat_simple$complaints, yrep = y_rep)

ppc_intervals(y = stan_dat_simple$complaints, yrep = y_rep, x = stan_dat_simple$traps) + 
  labs(x = "Number of bait stations", y = "Number of complaints")

ppc_stat_grouped(y = stan_dat_simple$complaints, yrep = y_rep, group = pest_data$building_id,
  stat = 'mean', binwidth = 0.1)






## hierarchical modeling
N_months <- length(unique(pest_data$date))

# Add some IDs for building and month
library(dplyr)

pest_data <- pest_data %>%
  mutate(
    building_fac = factor(building_id, levels = unique(building_id)),
    building_idx = as.integer(building_fac),
    ids = rep(1:N_months, N_buildings),
    mo_idx = lubridate::month(date)
  )

# Center and rescale the building specific data
building_data <- pest_data %>%
    select(building_idx, live_in_super, age_of_building,
      total_sq_foot, average_tenant_age, monthly_average_rent) %>% 
    unique() %>%
    arrange(building_idx) %>% 
    select(-building_idx) %>% 
    scale(scale=FALSE) %>%
    as.data.frame() %>%
    mutate( # scale by constants
      age_of_building = age_of_building / 10,
      total_sq_foot = total_sq_foot / 10000,
      average_tenant_age = average_tenant_age / 10,
      monthly_average_rent = monthly_average_rent / 1000
    ) %>%
    as.matrix()

# Make data list for Stan
stan_dat_hier <- with(pest_data, 
  list(complaints = complaints, 
       traps = traps, 
       N = length(traps), 
       J = N_buildings, 
       M = N_months,
       log_sq_foot = log(pest_data$total_sq_foot/1e4), 
       building_data = building_data[,-3],
       mo_idx = as.integer(as.factor(date)), 
       K = 4, 
       building_idx = building_idx))

## Compile and fit the hierarchical model
comp_model_NB_hier <- stan_model('hier_NB_regression.stan')

fitted_model_NB_hier <- sampling(comp_model_NB_hier,
  data = stan_dat_hier, chains = 4, cores = 4, iter = 4000)

sims_hier_NB <- extract(fitted_model_NB_hier)
print(fitted_model_NB_hier, pars = c('sigma_mu','beta','alpha','phi','mu'))

mcmc_trace(as.array(fitted_model_NB_hier,pars = 'sigma_mu'), 
  np = nuts_params(fitted_model_NB_hier), window = c(500,1000)
)

scatter_with_divs <- mcmc_scatter(as.array(fitted_model_NB_hier),
  pars = c("mu[4]", 'sigma_mu'), transform = list('sigma_mu' = "log"), np = nuts_params(fitted_model_NB_hier)
)
scatter_with_divs


N_sims <- 1000
log_sigma <- rep(NA, N_sims)
theta <- rep(NA, N_sims)
for (j in 1:N_sims) {
  log_sigma[j] <- rnorm(1, mean = 0, sd = 1)
  theta[j] <- rnorm(1, mean = 0, sd = exp(log_sigma[j]))
}
draws <- cbind("mu" = theta, "log(sigma_mu)" = log_sigma)
mcmc_scatter(draws)

parcoord_with_divs <- mcmc_parcoord(as.array(fitted_model_NB_hier, pars = c("sigma_mu", "mu")),
  np = nuts_params(fitted_model_NB_hier))
parcoord_with_divs

## Reparameterizing and rechecking diagnostics
comp_model_NB_hier_ncp <- stan_model('hier_NB_regression_ncp.stan')
fitted_model_NB_hier_ncp <- sampling(comp_model_NB_hier_ncp, data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.95))
print(fitted_model_NB_hier_ncp, pars = c('sigma_mu','beta','alpha','phi','mu'))

scatter_no_divs <- mcmc_scatter(as.array(fitted_model_NB_hier_ncp),
  pars = c("mu[4]", 'sigma_mu'), transform = list('sigma_mu' = "log"), 
  np = nuts_params(fitted_model_NB_hier_ncp))
bayesplot_grid(scatter_with_divs, scatter_no_divs, 
               grid_args = list(ncol = 2), ylim = c(-11, 1))

parcoord_no_divs <- mcmc_parcoord(
  as.array(fitted_model_NB_hier_ncp, pars = c("sigma_mu", "mu")),
  np = nuts_params(fitted_model_NB_hier_ncp)
)
bayesplot_grid(parcoord_with_divs, parcoord_no_divs, ylim = c(-3, 3))

sims_NB_hier_ncp <- extract(fitted_model_NB_hier_ncp, pars = c('y_rep','inv_phi'))

y_rep <- as.matrix(fitted_model_NB_hier_ncp, pars = "y_rep")
ppc_dens_overlay(stan_dat_hier$complaints, y_rep[1:200,])

ppc_stat_grouped(y = stan_dat_hier$complaints, 
  yrep = y_rep, group = pest_data$building_id, stat = 'mean', binwidth = 0.5)

ppc_stat_grouped(y = stan_dat_hier$complaints, 
  yrep = y_rep, group = pest_data$building_id, stat = 'sd', binwidth = 0.5)

ppc_intervals(y = stan_dat_hier$complaints, yrep = y_rep, x = stan_dat_hier$traps) + 
  labs(x = "Number of bait stations", y = "Number of complaints")


mean_y_rep <- colMeans(y_rep)
mean_inv_phi <- mean(as.matrix(fitted_model_NB_hier_ncp, pars = "inv_phi"))
std_resid <- (stan_dat_hier$complaints - mean_y_rep) / sqrt(mean_y_rep + mean_y_rep^2*mean_inv_phi)
qplot(mean_y_rep, std_resid) + hline_at(2) + hline_at(-2)

ppc_rootogram(stan_dat_hier$complaints, yrep = y_rep)


stan_dat_hier <- readRDS('pest_data_longer_stan_dat.RDS')
comp_model_NB_hier_slopes <- stan_model('hier_NB_regression_ncp_slopes_mod.stan')

fitted_model_NB_hier_slopes <- sampling(comp_model_NB_hier_slopes,
  data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.95))

mcmc_hist(as.matrix(fitted_model_NB_hier_slopes, pars = "sigma_kappa"), binwidth = 0.005)


print(fitted_model_NB_hier_slopes, pars = c('kappa','beta','alpha','phi','sigma_mu','sigma_kappa','mu'))

mcmc_hist(as.matrix(fitted_model_NB_hier_slopes, pars = "beta"), binwidth = 0.005)

y_rep <- as.matrix(fitted_model_NB_hier_slopes, pars = "y_rep")
ppc_dens_overlay(y = stan_dat_hier$complaints, yrep = y_rep[1:200,])

## time-varying effects and structured priors
select_vec <- which(stan_dat_hier$mo_idx %in% 1:12)
ppc_stat_grouped(y = stan_dat_hier$complaints[select_vec], 
  yrep = y_rep[,select_vec], group = stan_dat_hier$mo_idx[select_vec], stat = 'mean') + xlim(0, 11)
comp_model_NB_hier_mos <- stan_model('hier_NB_regression_ncp_slopes_mod_mos.stan')

fitted_model_NB_hier_mos <- sampling(comp_model_NB_hier_mos, data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.9))

y_rep <- as.matrix(fitted_model_NB_hier_mos, pars = "y_rep")
ppc_dens_overlay(y = stan_dat_hier$complaints, yrep = y_rep[1:200,])

select_vec <- which(stan_dat_hier$mo_idx %in% 1:12)
ppc_stat_grouped(y = stan_dat_hier$complaints[select_vec], 
  yrep = y_rep[,select_vec], group = stan_dat_hier$mo_idx[select_vec], stat = 'mean') 

rho_draws <- cbind(
  2 * rbeta(4000, 10, 5) - 1, # draw from prior
  as.matrix(fitted_model_NB_hier_mos, pars = "rho")
)
colnames(rho_draws) <- c("prior", "posterior")
mcmc_hist(rho_draws, freq = FALSE, binwidth = 0.025, facet_args = list(nrow = 2)) + xlim(-1, 1)
# (2) overlay prior density curve on posterior draws
gen_rho_prior <- function(x) {
  alpha <- 10
  beta <- 5
  a <- -1
  c <- 1
  lp <- (alpha - 1) * log(x - a) + (beta - 1) * log(c - x) - 
    (alpha + beta - 1) * log(c - a) - lbeta(alpha, beta)
  return(exp(lp))
}
mcmc_hist(as.matrix(fitted_model_NB_hier_mos, pars = "rho"),
  freq = FALSE, binwidth = 0.01) + overlay_function(fun = gen_rho_prior) + xlim(-1,1)

print(fitted_model_NB_hier_mos, pars = c('rho','sigma_mu','sigma_kappa','gamma'))

ppc_intervals(y = stan_dat_hier$complaints, yrep = y_rep, x = stan_dat_hier$traps) + 
  labs(x = "Number of bait stations", y = "Number of complaints")

## Using our model: Cost forecasts
comp_rev <- stan_model('stan/hier_NB_regression_ncp_slopes_mod_mos_predict.stan')
print(comp_rev)

rev_model <- sampling(comp_rev, data = stan_dat_hier, cores = 4, control = list(adapt_delta = 0.9))
N_traps <- 20
costs <- 10 * (0:N_traps) 

N_months_forward <- 12
N_months_labor <- N_months_forward / 2
hourly_rate_low <- 20
hourly_rate_high <- 30
costs <- costs + 
  (0:N_traps < 5 & 0:N_traps > 0) * (N_months_labor * hourly_rate_low) + 
  (0:N_traps >= 5 & 0:N_traps < 10) * (N_months_labor * (hourly_rate_low + 1 * hourly_rate_high)) + 
  (0:N_traps >= 10 & 0:N_traps < 15) * (N_months_labor * (hourly_rate_low + 2 * hourly_rate_high)) + 
  (0:N_traps >= 15) * (N_months_labor * (hourly_rate_low + 3 * hourly_rate_high))

# extract as a list for convenience below
sims_rev <- rstan::extract(rev_model)

# total and mean profit
tot_profit <- sweep(sims_rev$rev_, 3, STATS = costs, FUN = '-')
mean_profit <- t(apply(tot_profit, c(2, 3), median))

# lower and upper ends of 50% central interval
lower_profit <- t(apply(tot_profit, c(2, 3), quantile, 0.25))
upper_profit <- t(apply(tot_profit, c(2, 3), quantile, 0.75))

profit_df <-
  data.frame(
    profit = as.vector(mean_profit),
    lower = as.vector(lower_profit),
    upper = as.vector(upper_profit),
    traps = rep(0:N_traps, times = N_buildings),
    building = rep(1:N_buildings, each = N_traps + 1)
  )
ggplot(data = profit_df, aes(x = traps, y = profit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + geom_line() +
  facet_wrap(~ building, scales = 'free_y', ncol = 2)

## Gaussian process instead of AR(1)
comp_model_NB_hier_gp <- stan_model('hier_NB_regression_ncp_slopes_mod_mos_gp.stan')
fitted_model_NB_hier_gp <- sampling(comp_model_NB_hier_gp, data = stan_dat_hier, chains = 4, cores = 4, control = list(adapt_delta = 0.9))
sims_gp <- rstan::extract(fitted_model_NB_hier_gp)
length_scale_draws <- cbind(prior = rgamma(4000, 10, 2), posterior = sims_gp$gp_len)
mcmc_areas(length_scale_draws)

noise_to_length_scale_ratio_draws <- cbind(prior = abs(rnorm(4000)) / rgamma(4000, 10, 2),
  posterior = sims_gp$sigma_gp / sims_gp$gp_len)
mcmc_areas(noise_to_length_scale_ratio_draws)

mo_ar_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_mos, pars = "mo"), prob = 0.5)
mo_gp_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "gp"), prob = 0.5)
plot_data <- bind_rows(mo_ar_intervals, mo_gp_intervals)
plot_data$prior <- factor(rep(c("AR1", "GP"), each = 36), levels = c("GP", "AR1"))
plot_data$time <- rep(1:36, times = 2)
ggplot(plot_data, aes(x = time, y = m, ymin = l, ymax = h, fill = prior)) + 
  geom_ribbon(alpha = 2/3)

# visualizing 50% intervals
mo_noise_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "mo_noise"), prob = 0.5)
gp_exp_quad_intervals <- mcmc_intervals_data(as.matrix(fitted_model_NB_hier_gp, pars = "gp_exp_quad"), prob = 0.5)
plot_data <- bind_rows(mo_noise_intervals, gp_exp_quad_intervals)
plot_data$time <- rep(1:36, times = 2)
plot_data$term <- factor(rep(c("Monthly Noise", "Smooth Trend"), each = 36), 
  levels = c("Smooth Trend", "Monthly Noise"))
ggplot(plot_data, aes(x = time, y = m, ymin = l, ymax = h, fill = term)) + 
  geom_ribbon(alpha = 0.5) + geom_line(aes(color = term), size = 0.5) + ylab(NULL)

