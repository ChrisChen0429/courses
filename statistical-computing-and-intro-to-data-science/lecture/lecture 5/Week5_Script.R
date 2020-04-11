# Class Script
# October 6

# Writing Functions


significant <- function(x) {
  # Input x should be a single p-value in [0,1]
  
  if (x <= 0.05) { return(TRUE) }
  else { return(FALSE) }
}

significant(0.1)
significant(0.01)



# A Robust Loss Function
# Inputs: A vector of numbers x
# Outputs: A loss vector with x^2 for small elements,
#          and 2|x| - 1 for large numbers

res_loss <- function(x) {
  loss_vec <- ifelse(x^2 > 1, 2*abs(x) - 1, x^2)
  return(loss_vec)
}

vec <- c(-0.5, 0.9, -3, 4)
res_loss(vec)





# Check Yourself

FiveTimesSum <- function(vec) {
  return(5*sum(vec))
}
FiveTimesSum(1:3)


# A Robust Loss Function
# Inputs: A vector of numbers x,
#         crossover location (c > 0)
# Outputs: A loss vector with x^2 for small elements,
#          and 2|x| - c for large numbers

res_loss <- function(x, c = 1) {
  loss_vec <- ifelse(x^2 > c, 2*abs(x) - c, x^2)
  return(loss_vec)
}

identical(res_loss(vec), res_loss(vec, c = 1))
res_loss(vec, c = 3.5)
res_loss(vec)

identical(res_loss(vec, c = 3.5), res_loss(vec))

identical(res_loss(x = vec, c = 2), res_loss(c = 2, x = vec))

res_loss(2, vec)

res_loss(vec, c = c(1, 1, 1, 5))
res_loss(vec, c = -1)


res_loss2 <- function(x, c = 1) {
  # Crossover location c should be single, positive number.
  stopifnot(length(c) == 1, c > 0)
  loss_vec <- ifelse(x^2 > c, 2*abs(x) - c, x^2)
  return(loss_vec)
}

res_loss2(vec, c = -1)

# Check Yourself

f <- function(vec, thresh, dir = "below") {
  
  stopifnot(dir == "below" | dir == "above")
  
  if (dir == "below") { 
    vec[vec < thresh] <- thresh 
  } else {
    vec[vec > thresh] <- thresh 
  }
  return(vec)
}


curve(res_loss2, from = -2, to = 2)




x <- 2
f <- function(y) {
  return(x + y)
}
f(1)

g <- function(y) {
  x <- 10
  return(x + y)
}
g(1)

g <- function(y) {
  f <- function(y) {
    return(x + y)
  }
  x <- 10
  return(f(y))
}
g(1)


# Everything in R is a function

3+2
'+'(3, 2)

x <- matrix(runif(100), ncol = 10)
x[, 2]
'['(x, , 2)



class(1)
class(runif)
class(function(x) x^2)
square <- function(x) x^2
class(square)

# Model Fit Example

gmp <- read.table("gmp.txt", as.is = TRUE, header = TRUE)

head(gmp)[1:3, ]

gmp$pop <- gmp$gmp/gmp$pcgmp

plot(gmp$pop, gmp$pcgmp, log = "x", xlab = "Population",
     ylab = "Per-capita Economic Output")



# beta_0 = 6611, beta_1 = 1/8

curve(6611*x^{1/8}, add = TRUE, col = "blue")





# Gradient Descent

est.exp <- function(beta, beta_0 = 6611, max.iter = 100, stop.deriv = 1/100,
                    deriv.step = 1/1000, step.scale = 1e-12, predictor = gmp$pcgmp,
                    response = gmp$pop) {
  iter  <- 0
  deriv <- Inf
  
  mse <- function(b) {
    return(mean((predictor - beta_0*response^b)^2))
  }

  for (i in 1:max.iter) {
    iter <- iter + 1
    deriv <- (mse(beta + deriv.step) - mse(beta))/deriv.step
    beta <- beta - step.scale*deriv
    if (abs(deriv) < stop.deriv) {break()}
  }
  fit <- list(beta = beta, iteration = iter, conv = (iter < max.iter))
  return(fit)
}


# Classification


library(ISLR)
head(Smarket, 3)

mean(Smarket$Lag1[Smarket$Direction == "Up"])
mean(Smarket$Lag1[Smarket$Direction == "Down"])

K      <- 5
L1.new <- 2
L2.new <- 4.25

# K = 5, new point (2, 4.25)

dists     <- sqrt( (Smarket$Lag1 - L1.new)^2 + (Smarket$Lag2 - L2.new)^2 )
neighbors <- order(dists)[1:K]
neigh.dir <- Smarket$Direction[neighbors]
choice    <- names(which.max(table(neigh.dir)))







