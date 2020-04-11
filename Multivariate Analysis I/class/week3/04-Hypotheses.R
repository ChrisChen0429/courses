# Example: x1 = height, x2 = weight

mu0 <- c(70, 170)
x.bar = c(71.45, 164.7)

# Assume population covariance matrix is given:
S <- matrix(c(20, 100, 100, 1000), 2, 2)

# Test statistic
z.obs <- 20*t(x.bar - mu0)%*%solve(S)%*%(x.bar - mu0)
z.obs
# Critical chi-sq
qchisq(0.95,df=2)

1-pchisq(z.obs,df=2)
# Conclusion: reject Ho

# Doing same test one mu at a time;
(z.obs1 <- 20*(x.bar[1]-mu0[1])^2/S[1,1])
(z.obs2 <- 20*(x.bar[2]-mu0[2])^2/S[2,2])
qchisq(0.95,df=1)
# Do not reject Ho!!

# When sigma is unknown: use t-test in the univariate case:

# Sample size n
n = 19
# Let's simulate some data:
# Set the seed if you want to have the same exact numbers as here:
set.seed(874)
(x = rnorm(n, m = 9, sd = 0.5))
hist(x)

# Suppose we want to test H0: mu = 10 vs. mu not equal 10 (that is, two-tail test)
# at alpha level of 0.05
t.test(x, m = 10)
# Conclusion:
# Since p-value < 0.05 reject H0
# Conclude mu is not 10

# How about left-tail alternative H1: mu < 10
t.test(x, m= 10, alt = "less")
# Still reject H0, but notice p-value is half of the two-tail

# Manual computation of the test statistic:
(t.obs = (mean(x) - 10)/(sd(x)/sqrt(n)))
# Matches with t.test output!

# How about manual p-value:
# Left-tail:
pt(t.obs, n-1)

# Two-tail:
2*(1-pt(abs(t.obs), n-1))

# Both match t.test output!
