series <- matrix(scan("Series1000.txt"), nrow=1000, ncol=135, byrow=TRUE)
T <- 135
N <- 1000
par(mar=c(3,3,2,0), tck=-.01, mgp=c(1.5,.5,0))
plot(c(1,T), range(series), bty="l", type="n", xlab="Time", ylab="series")
for (n in 1:N){
  lines(1:T, series[n,], lwd=.5)
}
slope <- rep(NA, N)
se <- rep(NA, N)
for (n in 1:N){
  data <- series[n,]
  time <- 1:T
  fit <- lm(data ~ time)
  slope[n] <- 100*coef(fit)[2]
  se[n] <- 100*se.coef(fit)[2]
}
plot(slope, se, bty="l", xlab="Slope", ylab="SE", pch=20)
hist(slope, xlab="Slope", breaks=seq(floor(10*min(slope)), ceiling(10*max(slope)))/10)


# fit a mixture model
y <- slope
K <- 3
mu <- c(0,-1,1)
data <- list(y=y, K=K, mu=mu)
fit <- stan("timeseries.stan", data=data)
print(fit)
