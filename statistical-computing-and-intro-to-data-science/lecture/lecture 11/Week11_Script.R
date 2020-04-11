# Class Script, Lecture 11
# November 17

# Selective Access

data(cats, package = "MASS")
head(cats)

# Only want, heart weights of male cats
head(cats$Hwt[cats$Sex == "M"])
head(cats$Hwt[cats$Sex != "M"])

head(cats[cats$Sex == "M", "Hwt"])

# Selective access using indexing

cats.subset <- sample(1:nrow(cats), size = nrow(cats)/2)
new.cats    <- cats[cats.subset, ]
head(new.cats)
dim(new.cats)

# A new way to do this.
boy.cats   <- cats[cats$Sex == "M", ]
boy.cats.1 <- subset(cats, Sex == "M")

# States data, 50 states with 9 variables
states <- data.frame(state.x77, Region = state.region)
unique(states$Region)

# Looking for states data from states in the south
states$Income["South"] # Doesn't do what we want

income.south <- c()
for (i in 1:50) {
  if (states$Region[i] == "South") {
    income.south <- c(income.south, states$Income[i])
  }
}

# The above is a bad way to get what we want
income.south.1 <- states$Income[states$Region == "South"]
income.south.2 <- subset(states,Region=='South')$Income
all(income.south.1==income.south.2)
# Check Yourself

sum(states$Frost >= 150)
row.names(states)[states$Frost >= 150]

avgs <- colSums(states[, 1:8])/nrow(states)
colSums(states[, 1:8] > rep(avgs, each = nrow(states)))

mat1 <- states[, 1:8] > matrix(rep(avgs, each = nrow(states)), ncol = 8)
mat2 <- states[, 1:8] > avgs #rep(avgs, 8*nrow(states))
all(mat1==mat2)
# Apply Functions


row.names(subset(states,Frost>=150))
colMeans(states[,1:8])
attributes(states)



# Max. entry in each column
apply(states[, 1:8], 2, max)
row.names(states)[apply(states[, 1:8], 2, which.max)]
apply(states[,1:8],2,which.max)
rownames(states)[apply(states[,1:8],MARGIN = 2,FUN = which.max)]


summary(states$Frost)
apply(states[, 1:8], 2, summary)


# Writing our own function
frow <- function(r) {
  val <- as.numeric(r[7])
  return(ifelse(r[9] == "Northeast", 0.5*val, val))
}

frost.fake <- apply(states, 1, frow)
mean(states$Frost[states$Region == "Northeast"])
mean(frost.fake[states$Region == "Northeast"])

top.3.names <- function(v, names.v) {
  names.v[order(v, decreasing = TRUE)[1:3]]
}

apply(states[, 1:8], 2, top.3.names, names.v = rownames(states))



cor.v1.v2 <- function(v1,v2=states$Frost){
        v1 <- as.numeric(v1)
        v2 <- as.numeric(v2)
        return(cor(v1,v2))
}

apply(states[,1:8],2,cor.v1.v2)


# Check Yourself

cor.v1.v2 <- function(v1, v2 = states[, "Frost"]) {
  return(cor(v1, v2))
}

cor.v1.v2(states$Life.Exp)
cor.v1.v2(states$Frost)
apply(states[, 1:8], 2, cor.v1.v2)
apply(states[, 1:8], 2, cor, states[, "Frost"])

# lapply, sapply

# Jackknife (leave-one-out mean)
mean.less.one <- function(i, vec) {
  return(mean(vec[-i]))
}

my.vec <- states$Frost
n      <- length(my.vec)

my.vec.jack <- rep(NA, n)
for (i in 1:n) {
  my.vec.jack[i] <- mean.less.one(i, my.vec)
}


my.vec.jack.2 <- lapply(1:n, mean.less.one, my.vec)
my.vec.jack.3 <- sapply(1:n, mean.less.one, my.vec)

# tapply -- Let's avg the frost variable in each region

mean(states$Frost[states$Region == "Northeast"])
tapply(states$Frost, states$Region, mean)

# mapply

rep(1, 4)
mapply(rep, 1:4, 4:1)
mapply(rep, c(1, 2, 3, 4), c(4, 3, 2, 1))



# Re-ordering data


cats$Hwt[order(cats$Hwt)]

hwt.order  <- order(cats$Hwt)
cats.order <- cats[hwt.order, ]


this.vec <- c(25, 13, 25, 77, 68)
order(this.vec)
this.vec[order(this.vec)]
sort(this.vec)
rank(this.vec)

# Merge Example

fha <- read.csv("fha.csv", na.string = "NA", 
                colClasses = c("character", rep("double", 3)))
nrow(fha)
head(fha)
colnames(fha)

ua <- read.csv("ua.txt", sep = ";")
nrow(ua)
head(ua)

dim(fha) # 498 rows
length(unique(fha$Population))

ua.pop.top498 <- sort(ua$POP, decreasing = TRUE)[1:nrow(fha)]
head(ua.pop.top498)
head(fha$Population)
all(ua.pop.top498==fha$Population)
# Option 1: Reorder area in ua by population, append to fha
ua.sort <- ua[order(ua$POP, decreasing = TRUE), ]
area    <- ua.sort$AREALANDSQMI[1:nrow(fha)]
df1     <- data.frame(fha, area)
colnames(df1) <- c("City", "Population", "Roads", "Mileage", "Area")
nrow(df1)

# Option 2: merge()
df2 <- merge(x = fha, y = ua, by.x = "Population", by.y = "POP")
head(df2)
tail(df2)

# Merge on city names
df2.1 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME")
nrow(df2.1)

df2.2 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME", all.x = TRUE)
nrow(df2.2)
df2.2$City[is.na(df2.2$POP)]

#Check Yourself

x <- 1:3
a <- cumprod(x)^(1/3)
exp((1/3)*sum(log(x)))
# Convert 1,000s of miles to miles
df1$Mileage <- 1000 * df1$Mileage
# Plot daily miles per person vs. area
plot(Mileage/Population ~ Area, data = df1, log = "x",
     xlab = "Miles driven (per person per day)",
     ylab = "City area (sq. miles)")
# Impressively flat regression line
abline(lm(Mileage/Population ~ Area, data = df1),
       col = "blue")

head(scale(cats[,-1],center = TRUE,scale = TRUE),3)

exp(colMeans(log(states[, 1:8])))


geom.mean <- function(vec) {
  return((prod(vec))^(1/n))
}

apply(states[, 1:8], 2, geom.mean)

# split() function -- split states df according to region
states.by.reg <- split(states, states$Region)
class(states.by.reg)
length(states.by.reg)
names(states.by.reg)
class(states.by.reg[[1]])

# For each, display first two rows

head(states.by.reg[[1]], 2)
lapply(states.by.reg, head, 2)

# For each region, average the eight numeric variables


mean.func <- function(df) {
  apply(df[, 1:8], 2, mean)
}
  
unlist(lapply(states.by.reg, mean.func))

# Aggregate
aggregate(states[, 1:8], list(states$Region), mean)
tapply(states[, 1:8], list(states$Region), mean)

grade.by.lit.median <- function(v1){
        apply(v1,2,median,na.rm=TRUE)
}
sapply(split(states[,1:8],f=states$state.division),grade.by.lit.median)

library(MASS)

plot(ecdf(pgamma(cats$Hwt, shape = a, scale = s)),
     main = "Calibration of gamma distribution for cat hearts")
abline(0, 1, col = "red")

