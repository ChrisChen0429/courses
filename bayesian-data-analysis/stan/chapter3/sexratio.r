sex_data <- list(y=8, sigma_y=3, mu_0=0, sigma_0=0.25)
fit1 <- stan("sexratio.stan", data=sex_data)
print(fit1)
