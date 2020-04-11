# Class Script
# September 22, 2017

# tug.org/begin.html


# Examples of for loops

for (i in 1:10) {
  print(i)
}

x <- c(5, 12, -3)
for (i in x) {
  print(i^2)
}

for (index in 1:3) {
  print(x[index]^2)
}

mat <- matrix(1:9, ncol = 3)
for (i in 1:3) {
  for (j in 1:3) {
    print(mat[i,j])
  }
}


i <- 1
while (i <= 10) {
  i <- i + 4
}

count <- 1
while (count <= 10) {
  count <- count - 1
}

walk <- 0
for (i in 1:100) {
  step <- sample(c(1, -1), size = 1)
  walk <- c(walk, walk[i] + step)
}
plot(walk, type = "l")

walk <- 0
loc  <- 0
while (loc < 3 & loc > -3) {
  step <- sample(c(1, -1), size = 1)
  loc  <- loc + step
  walk <- c(walk, loc)
}
walk


# If, else statements

for (i in seq(4)) {
  if (i %% 2 == 0) {
    print(log(i))
  } else {
    print("Odd")
  }
}


x <- runif(1, min = 0, max = 10)
if (x > 3) {
  y <- 10
  print("x is greater than 3")
} else {
  y <- 0
  print("x is less than 3")
}


install.packages("matlab")
library(matlab)
total <- 0
for (i in 1:10) {
  if(isprime(i)) {
    total <- total + i
    print(total)
  }
}






# Vectorized Operations: Adding two vectors

u <- c(1, 2, 3)
v <- c(10, -20, 30)

sum_vec <- vector(mode = "numeric", length = length(u))
for (i in 1:length(u)) {
  sum_vec[i] <- u[i] + v[i]
}
sum_vec

sum_vec2 <- u + v
sum_vec2

# ifelse() function

for (i in seq(4)) {
  if (i %% 2 == 0) {
    print(log(i))
  } else {
    print("Odd")
  }
}

vec <- ifelse(seq(4) %% 2 == 0, log(seq(4)), "Odd")




vals <- rnorm(1e6)

system.time(trunc  <- ifelse(vals > 0, vals, 0))
system.time(trunc2 <- vals * (vals > 0))

all(trunc == trunc2)


# Pre-allocation

vals <- 0
n    <- 50000
system.time({
for (i in 1:n) {
  vals <- c(vals, i)
}
})  
length(vals)
head(vals)


vals <- 0
system.time({
for (i in 1:n) {
  vals[i] <- i
}
})




system.time({
  vals <- rep(0, n)  
  for (i in 1:n) {
    vals[i] <- i
  }
})

system.time({
  vals <- 1:n
})









count <- 0
while (count < 10) {
  count <- count + 1
  print(count)
}

# Apply Functions

mat <- matrix(1:12, ncol = 6)
mat

colSums(mat)
apply(mat, 2, sum)
apply(mat, 1, sum)


x <- matrix(rnorm(200), nrow = 20, ncol = 10)
apply(x, 2, mean)
apply(x, 1, mean)

apply(x, 2, quantile, probs = c(0.25, 0.75))
quantile(x[, 1], probs = c(0.25, 0.75))


# lapply() and sapply()

vec1 <- c(1.1, 3.4, 2.4, 3.5)
vec2 <- c(1.1, 3.4, 2.4, 10.8)

not_robust <- list(v1 = vec1, v2 = vec2)

lapply(not_robust, mean)
sapply(not_robust, mean)

unlist(lapply(not_robust, mean))



HC <- scan("HonorCode.txt", what = "")
HC <- factor(HC, levels = unique(HC))

findwords <- function(tv) {
  words <- split(1:length(tv), tv)
  return(words)
}

wl <- findwords(HC)
wl[1:3]


freq_list <- function(wordlist) {
  freqs <- sapply(wordlist, length)
  return(wordlist[order(freqs)])
}


head(wl)
head(sapply(wl, length))


# tapply() Examples

vec    <- c(rnorm(10), runif(10), rnorm(10, mean = 1))
groups <- factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))

tapply(vec, groups, mean)
tapply(vec, groups, range)


# EDA

diamonds <- read.csv("diamonds.csv", as.is = TRUE)
diamonds$cut <- factor(diamonds$cut)
diamonds$color <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

# Bargraph for the diamond cut
table(diamonds$cut)
names(table(diamonds$cut))

barplot(height = table(diamonds$cut), names.arg = names(table(diamonds$cut)))

diamonds$cut <- factor(diamonds$cut, level = c("Fair", "Good", "Very Good", "Premium", "Ideal"))

table(diamonds$cut)
names(table(diamonds$cut))

barplot(height = table(diamonds$cut), names.arg = names(table(diamonds$cut)))





boxplot(diamonds$price ~ diamonds$cut)
boxplot(price ~ cut, data = diamonds, ylab = "Price", xlab = "Cut")

plot(x = diamonds$carat, y = diamonds$price, xlab = "Carats", ylab = "Price ($)")








# Check Yourself Solutions:

walk <- 0
loc  <- 0
while(loc < 3 & loc > -3) {
  step <- sample(c(-1, 1), size = 1)
  loc  <- loc + step
  walk <- c(walk, loc)
}
plot(walk, type = "l")



library(matlab)    
total <- 0
for (i in 1:10) {
  if(isprime(i)) {
    total <- total + i
  }
}



tapply(diamonds$price, diamonds$color, mean)



medals <- read.csv("all_medalists.csv")
medals_new <- medals[medals$Country == "United States" & medals$Medal == "Gold", ]
our_table <- table(medals_new$Year)
barplot(height = table(medals_new$Year), names.arg = names(table(medals_new$Year)))
plot(as.numeric(names(our_table)), our_table, xlab = "Year", ylab = "Number of Golds")