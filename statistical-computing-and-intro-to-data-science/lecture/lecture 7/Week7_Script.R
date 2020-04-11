# Class Script, Lecture 7
# October 20

# LCG

new.random <- function(a = 5, c = 12, m = 16, seed, len) {
  seq    <- rep(NA, len)
  seq[1] <- seed
  for (i in 2:len) {
    seq[i] <- (a*seq[i-1] + c) %% m
  }
  return(seq)
}
new.random(seed = 10, len = 20)
new.random(a = 131, c = 7, m = 16, seed = 10, len = 20)
new.random(a = 131-16,c=7,m=16,seed=10,len=20 )
new.random(a = 131-10,c=7,m=16,seed=10,len=20)
new.random(a = 129, c = 7, m = 16, seed = 10, len = 20)
round(new.random(a = 1664545, c = 1013904223, m = 2^{32}, seed = 10, len = 20)/2^{32}, 3)



# Simulations with Common Distributions

# (1/sqrt(2*pi))*exp(-x^2/2)
dnorm(2, mean = 0, sd = 1)
1/sqrt(2*pi)
(1/sqrt(2*pi))*exp(-2^2/2)

rnorm(1)
rnorm(5)
x <- rnorm(10, mean = 100, sd = 1)
summary(x)

library(ggplot2)
ggplot(data.frame(x = c(-5, 5))) +
  stat_function(mapping = aes(x = x), fun = dnorm) +
  labs(title = "Standard Normal Density")

curve(dnorm,from = -5,to=5)

# Pr(Z < 0)
pnorm(0)
# Pr(-1.96 < Z < 1.96)
pnorm(1.96) - pnorm(-1.96) 

# Pr(Z < ?) = 0.5
qnorm(.5)
pnorm(0)

# Pr(Z < ?) = 0.975
qnorm(0.975)

# Binomial Example
# Pr(X <= 190); X ~ Bin(n = 1000, p = 0.2)
val <- 190; n <- 1000; p <- 0.2
correction <- (val - n*p)/(sqrt(n*p*(1-p)))

pnorm(correction)
pbinom(val, size = n, prob = p)
sum(dbinom(0:190, size = n, prob = p))

x <- rbinom(500, size = n, prob = p)
ggplot(data.frame(x)) +
  geom_histogram(aes(x = x, y = ..density..)) +
  stat_function(aes(x = x), fun = dnorm,
                args = list(mean = 200, sd = sqrt(160)), color = "red")

ggplot(data.frame (x=c(5,-5)))+
        stat_function(aes(x=x,col='df=1'),fun = dt,args = list(df=1))+
        stat_function(aes(x=x,col='df=2'),fun = dt,args = list(df=2))+
        stat_function(aes(x=x,col='df=5'),fun = dt,args = list(df=5))+
        stat_function(aes(x=x,col='df=30'),fun = dt,args = list(df=30))+
        stat_function(aes(x=x,col='df=100'),fun = dt,args = list(df=100))
        


# Student's-t
ggplot(data.frame(x = c(-5, 5))) +
  stat_function(aes(x=x), fun = dnorm, color = "red") +
  stat_function(aes(x=x, linetype = "df=1"), fun = dt, args = list(df = 1)) +
  stat_function(aes(x=x, linetype = "df=2"), fun = dt, args = list(df = 2)) +
  stat_function(aes(x=x, linetype = "df=5"), fun = dt, args = list(df = 5)) +
  stat_function(aes(x=x, linetype = "df=30"), fun = dt, args = list(df = 30)) +
  stat_function(aes(x=x, linetype = "df=100"), fun = dt, args = list(df = 100))



# Check Yourself
pgamma(2, shape = 2, rate = 1)     # P(0 < X < 2)
1 - pgamma(2, shape = 2, rate = 1) # P(X > 2)


random_variables <- rgamma(100000,shape = 2,scale = 1)
p <- mean(random_variables>2)

ggplot(data.frame(x = c(0, 10))) +
  stat_function(aes(x = x, color = "alpha = 2"), fun = dgamma, 
                args = list(shape = 2, rate = 1)) +
  stat_function(aes(x = x, color = "alpha = 3"), fun = dgamma, 
                args = list(shape = 3, rate = 1)) +
  stat_function(aes(x = x, color = "alpha = 4"), fun = dgamma, 
                args = list(shape = 4, rate = 1)) +
  stat_function(aes(x = x, color = "alpha = 5"), fun = dgamma, 
                args = list(shape = 5, rate = 1)) +
  stat_function(aes(x = x, color = "alpha = 6"), fun = dgamma, 
                args = list(shape = 6, rate = 1)) +
  labs(title = "Gamma Density Function", color = "Legend")

# Simulating Linear Models

e <- rnorm(100,mean = 0,sd=2)
x <- rnorm(100,mean = 0,sd=1)
y <- 0.5 + 2 * x + e
plot(y,x)

# Simulations Check Yourself 1
x <- rnorm(100)
e <- rnorm(100, sd = 2)
y <- 0.5 + 2*x + e
ggplot(data.frame(x = x, y = y)) +
  geom_point(aes(x = x, y = y))

# The sample() function
sample(1:10, 4)
sample(1:10)
sample(1:10, 7, replace = TRUE)
sample(letters, 12)

# Prob distribution: X = 1 (w.p. 0.1), X = 2 (0.2), X = 3 (0.7)
p <- c(0.1, 0.2, 0.7)
x <- sample(1:3, size = 1000, prob = p, replace = TRUE)
table(x)/1000

# Simulations Check Yourself 3
n     <- 100
rolls <- sample(1:6, n, replace = TRUE)
table(rolls)

rolls <- floor(runif (n, min = 0, max = 6))
table(rolls+1)


rolls <- round(runif(100000, min = 0.5, max = 6.5))


random_variables <- table(sample(1:3,size=100,replace = TRUE,prob = c(0.5,0.25,0.25)))/100
random_variables        
        
        
# Inverse Transform Method

lambda <- 2
n <- 1000
u <- runif(n) # Simulating uniforms

# x <- F^{-1}(u)
Finverse <- function(u, lambda) {
  stopifnot(u > 0 & u < 1)
  return(-(1/lambda)*log(1-u))
}
x <- Finverse(u, lambda)

ggplot(data.frame(x)) +
  geom_histogram(aes(x = x, y = ..density..)) +
  stat_function(aes(x=x), fun = dexp, 
                args = list(rate = 2), color = "red")

# Simulations: Uncommon Dists Check Yourself 1

# F(x) = int_0^x f(t) dt = int_0^x 3t^2 dt = x^3.
# F(x) = 1 if x < 0 and F(x) = 1 if x > 1
# u=x^3 then  x=u^{1/3}

n         <- 1000
u         <- runif(n) # Simulate uniforms

F.inverse <- function(u) {return(u^(1/3))}
x         <- F.inverse(u)

# x <- u^(1/3)

ggplot(data.frame(x)) +
  geom_histogram(aes(x = x, y = ..density..)) +
  stat_function(aes(x = x), fun = function(x) {3*x^2}, color = "red") +
  labs(title = "Inverse Transform Method")


# Rejection Algorithm (semicircle example)

p <- function(x) {sqrt(1 - x^2)}
generate.one <- function(p_func) {
  val <- NULL
  while (is.null(val)) {
    x <- runif(1, min = -1, max = +1)
    u <- runif(1)
    if (u <= p_func(x)) {val <- x}
  }
  return(val)
}

n    <- 1000
samp <- rep(NA, n)
for (i in 1:n) {samp[i] <- generate.one(p)}
ggplot(data.frame(samp)) +
  geom_histogram(aes(x = samp))
hist(samp)


# Envelope Method
plot(c(0, 1), c(0, 3), ty = "n", xlab = "x") 
curve(dbeta(x, 3, 6), add = TRUE, col = "red")
lines(c(0,0,1,1), c(0, 2.6, 2.6, 0))

x1 <- runif(3000, min = 0, max = 1)
y1 <- runif(3000, min = 0, max = 2.6)
selected <- y1 < dbeta(x1, 3, 6)
points(x1, y1, col = 1+selected, cex = 0.1)

mean(selected)








# Simulations: MC Check Yourself 1

n <- 10000
mean(rexp(n, rate = 1/3) < 3)
pexp(3, rate = 1/3)


# Simulations: MC Check Yourself 2

num <- 5000; n <- 16; p <- 0.5
b   <- rbinom(num, n, p)
mean(b) # Should be close to np = 8
sd(b)   # Should be close to sqrt(np(1-p)) = 2

num   <- 5000000; n <- 1000000; p <- 0.5
b.big <- rbinom(num, n, p)

b.big <- (b.big - mean(b.big))/sd(b.big)

hist(b, breaks = 100, prob = TRUE)
hist(b.big, breaks = 100, prob = TRUE)


n1 <- 10000; n2 <- 1000
estvec1 <- rep(NA, 1000); estvec2 <- rep(NA, 1000)
for (i in 1:1000) {
        norms1 <- rnorm(n1, sd = 1/sqrt(2))
        estvec1[i] <- mean(sin(norms1)^2)
}
for (i in 1:1000) {
        norms2 <- rnorm(n2, sd = 1/sqrt(2))
        estvec2[i] <- mean(sin(norms2)^2)
}
df <- data.frame(estimates = c(estvec1, estvec2),
                 n = c(rep(n1, 1000), rep(n2, 1000)))
ggplot(df) +
        geom_histogram(aes(x = estimates)) + facet_wrap(~ n, ncol = 2)


n <- 10000000
norms <- rnorm(n)
est <- sqrt(2*pi) * mean(sin(norms)^2 * exp(-(1/2)*norms^2))
est

u <- runif(10000000)
est <- mean(sin(u)^2 * exp(-u^2))
est
