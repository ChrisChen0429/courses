# Class Script, Lecture 6
# October 13



# Build KNN Classifier

library(ISLR)

KNNclass <- function(NewPoint, K=5, Lags = Weekly[, 2:6], Dir = Weekly$Direction) {
  n <- nrow(Lags)
  stopifnot(length(NewPoint) == 5, ncol(Lags) == 5, K <= n)
  
  dists      <- rowSums((Lags - rep(NewPoint, each = n))^2)
  neighbors  <- order(dists)[1:K]
  neighb.dir <- Dir[neighbors]
  choice     <- names(which.max(table(neighb.dir)))
  return(choice)
}

NewPoint <- c(-.5, .5, -.5, -.5, .5)
KNNclass(NewPoint)

test  <- Weekly[Weekly$Year >= 2009, ]
train <- Weekly[Weekly$Year < 2009, ]

n.test      <- nrow(test)
predictions <- rep(NA, n.test)

for (i in 1:n.test) {
  newp           <- as.numeric(test[i, 2:6])
  predictions[i] <- KNNclass(newp, Lags = train[, 2:6], Dir = train$Direction)
}

test.error <- mean(predictions != test$Direction)
test.error

# Divide the data into k=9 folds

k    <- 9
nums <- rep(1:k, each = nrow(Weekly)/k)
fold <- sample(nums)

# Iterate over the folds, getting an estimate of the test error for K=5 

fold.error <- rep(NA, k)
for (j in 1:k) {
  test  <- Weekly[fold == j, ]
  train <- Weekly[fold != j, ]
  
  n.test      <- nrow(test)
  predictions <- rep(NA, n.test)
  
  for (i in 1:n.test) {
    newp           <- as.numeric(test[i, 2:6])
    predictions[i] <- KNNclass(newp, Lags = train[, 2:6], Dir = train$Direction)
  }
  
  fold.error[j] <- mean(predictions != test$Direction)
}
approx.error <- mean(fold.error)
approx.error


# Iterate over parameters K, getting an estimate of the test error for each

K            <- seq(1, 55, by = 2)
approx.error <- rep(NA, length(K))
count        <- 1

for (kay in K) {
  fold.error <- rep(NA, k)
  for (j in 1:k) {
    test  <- Weekly[fold == j, ]
    train <- Weekly[fold != j, ]
    
    n.test      <- nrow(test)
    predictions <- rep(NA, n.test)
    
    for (i in 1:n.test) {
      newp           <- as.numeric(test[i, 2:6])
      predictions[i] <- KNNclass(newp, K = kay, Lags = train[, 2:6], Dir = train$Direction)
    }
    
    fold.error[j] <- mean(predictions != test$Direction)
  }
  approx.error[count] <- mean(fold.error)
  count <- count + 1
}


approx.error[1:4]
plot(K, approx.error, xlab = "K", lty = 1, ylab = "Approximate Test Error", 
     main = "Predicting Market Direction", pch = 1)
lines(K, approx.error, lty = 1)


# Plotting with base R

diamonds <- read.csv("diamonds.csv", as.is = TRUE)
lev_vec  <- c("Fair", "Good", "Very Good", "Premium", "Ideal")
diamonds$cut <- factor(diamonds$cut, level = lev_vec)
diamonds$color <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

set.seed(1)
rows <- dim(diamonds)[1]
diam <- diamonds[sample(1:rows, 1000), ]

plot(log(diam$carat), log(diam$price), col = diam$cut)
legend("bottomright", legend = levels(diam$cut), fill = 1:length(levels(diam$cut)))

plot(1:5, 1:5, col = "red")
plot(1:5, 1:5, col = "red", pch = 19)
plot(1:5, 1:5, col = 3, pch = 19)
plot(1:5, 1:5, col = 1:5, pch = 19)
legend("bottomright", legend = 1:5, fill = 1:5)

plot(log(diam$carat), log(diam$price), col = diam$cut)
abline(a = 8, b = 0, col = "orange", lty = 2)
lm1 <- lm(log(diam$price) ~ log(diam$carat))
abline(lm1)

cuts        <- levels(diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut  <- diam$cut == i
  this_data <- diam[this_cut, ]
  this_lm   <- lm(log(this_data$price) ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1 
}


points(-0.4, 6.8, pch = "*", col = "purple", cex = 1.5)
text(-0.4, 6.8 - 0.2, "New Diamond", cex = .5)


# GGplot 

library(ggplot2)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))

table(mpg$class, mpg$drv)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")



ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 2)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~class)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x = 3, y = 30), color = "purple") +
  geom_text(mapping = aes(x = 3, y = 31, label = "New Point"), size = 4) +
  labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")

plot(diamonds$carat, diamonds$price)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/10)







# Check Yourself:

diam$color2 <- "bad"
diam$color2[diam$color == "D" | diam$color == "E" | diam$color == "F"] <- "good"
diam$color2[diam$color %in% c("D", "E", "F")] <- "good"
diam$color2 <- as.factor(diam$color2)

plot(log(diam$carat), log(diam$price), col = diam$color2)
legend("bottomright", legend = levels(diam$color2),
       fill = 1:length(levels(diam$color2)))

cols        <- levels(diam$color2)
col_counter <- 1

for (i in cols) {
  this_col    <- diam$color2 == i
  this_data   <- diam[this_col, ]
  this_lm     <- lm(log(this_data$price) ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}




ggplot(data = mpg)
ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))

head(mpg)
table(mpg$class, mpg$drv)


ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))


ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy), color="blue")

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = cty))


ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ class)
ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ city, nrow = 2)