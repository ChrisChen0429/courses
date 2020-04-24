# Example 11.10 on p. 612:

# Group 1
(x1 = matrix(c(-2, 0, -1, 5, 3, 1), 3, 2))
n1 = nrow(x1)
(x1.bar = colMeans(x1))
(S1 = cov(x1))

# Group 2
(x2 = matrix(c(0, 2, 1, 6, 4, 2), 3, 2))
n2 = nrow(x2)
(x2.bar = colMeans(x2))
(S2 = cov(x2))

# Group 3
(x3 = matrix(c(1, 0, -1, -2, 0, -4), 3, 2))
n3 = nrow(x3)
(x3.bar = colMeans(x3))
(S3 = cov(x3))

# Priors:
p1 = p2 = 0.25
p3 = 0.5

# Observation that needs to be classified:
x = c(-2, -1)

# Plot
plot(x1, col = 1, ylim = c(-4, 6), xlim = c(-2,2), pch = 16)
points(x2,col= 2, pch = 16)
points(x3,col= 3, pch = 16)
points(x[1], x[2],col = 4, pch = 2)

# Pooled sample covariance matrix:
(Sp <- (1/(n1+n2+n3-3))*((n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3))
solve(Sp)

# Searching for highest discriminant score L:
t(x1.bar) %*% solve(Sp)
t(x1.bar) %*% solve(Sp) %*% x1.bar
(l1 = t(x1.bar) %*% solve(Sp) %*% x + log(p1) - t(x1.bar) %*% solve(Sp) %*% x1.bar/2)

t(x2.bar) %*% solve(Sp)
t(x2.bar) %*% solve(Sp) %*% x2.bar
(l2 = t(x2.bar) %*% solve(Sp) %*% x + log(p2) - t(x2.bar) %*% solve(Sp) %*% x2.bar/2)

t(x3.bar) %*% solve(Sp)
t(x3.bar) %*% solve(Sp) %*% x3.bar
(l3 = t(x3.bar) %*% solve(Sp) %*% x + log(p3) - t(x3.bar) %*% solve(Sp) %*% x3.bar/2)
# Since L3 is the highest we classify x as belonging to Group 3


## Example 11.2 on p. 614
# Read data from file T11-6.DAT
admission = read.table("T11-6.DAT")
colnames(admission) = c("GPA", "GMAT", "Status")

# Scatterplot by group:
library(car)
scatterplot(GPA ~ GMAT | Status, data = admission, regLine = FALSE, smooth = FALSE)

# Sample sizes:
(n1 = sum(admission$Status == 1))
(n2 = sum(admission$Status == 2))
(n3 = sum(admission$Status == 3))

# Sample means:
(x1.bar = colMeans(admission[admission$Status == 1, 1:2]))
(x2.bar = colMeans(admission[admission$Status == 2, 1:2]))
(x3.bar = colMeans(admission[admission$Status == 3, 1:2]))
(x.bar = colMeans(admission[, 1:2]))

# Covariance matrices:
S1 = cov(admission[admission$Status == 1, 1:2])
S2 = cov(admission[admission$Status == 2, 1:2])
S3 = cov(admission[admission$Status == 3, 1:2])
(Sp = (Sp <- (1/(n1+n2+n3-3))*((n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3)))

# New observation to be classified:
xnew = data.frame(GPA = 3.21, GMAT = 497)

# Squared distances:
(d1 = (as.matrix(xnew) - x1.bar) %*% solve(Sp) %*% t(as.matrix(xnew) - x1.bar))
(d2 = (as.matrix(xnew) - x2.bar) %*% solve(Sp) %*% t(as.matrix(xnew) - x2.bar))
(d3 = (as.matrix(xnew) - x3.bar) %*% solve(Sp) %*% t(as.matrix(xnew) - x3.bar))
# Assign to Group 3 (borderline admit) because the distance is the smallest
# Note the numbers are slightly off from p. 615, because the book used rounding

# To verify the SAS output on p. 616 we can calculate the coefficient vectors:
solve(Sp) %*% x1.bar
solve(Sp) %*% x2.bar
solve(Sp) %*% x3.bar

# Let's do the discrimination with lda function
library(MASS)
ldamod = lda(Status ~ ., data= admission, prior=rep(1/3, 3))
ldamod
# Note: the number of discriminant functions is min(g-1, p) = 2
# First linear discriminant function = -5.0172*GPA - 0.00085*GMAT
# Second linear discriminant function = 1.854*GPA -0.01448967*GMAT

predict(ldamod)
table(Predicted=predict(ldamod)$class, Actual = admission$Status)
# Accuracy:
mean(predict(ldamod)$class == admission$Status)

# Visual with package klaR
#install.packages('klaR')
library(klaR)
admission$Status = as.factor(admission$Status)
partimat(Status~.,data=admission,method="lda") 

# Predicting the case GPA = 3.21, GMAT = 497
predict(ldamod, xnew)
# Goes to Group 3 as we already knew


## Wine example
#install.packages('rattle')
data(wine, package='rattle')
attach(wine)
head(wine)
# There are 3 types of wine and 13 chemical concentrations

# Some scatterplots
scatterplotMatrix(wine[2:6])

# LDA
(wine.lda = lda(Type ~ ., data=wine))
# Note first discriminant achieves 68.75% separation and second 31.25%

# Plot of separation:
plot(wine.lda)
abline(h=0)
abline(v=0)
# First discriminant separates Groups 1 and 3 very well, but not Group 2 from them
# Second discriminant separates Groups 1 and 3 from Group 2 

wine.lda.values = predict(wine.lda)
# Plot of separation for each discriminant
ldahist(data = wine.lda.values$x[,1], g=Type)
ldahist(data = wine.lda.values$x[,2], g=Type)

table(Predicted= wine.lda.values$class, Actual= Type)
# We achieved perfect separation!!

# Quadratic model:
qda.fit <- qda(Type ~ ., data= wine)
qda.fit
qda.class <- predict(qda.fit)$class
table(qda.class, Type)
# Same as linear!

# Exercise:
# Use the built-in dataset iris
# a) Use the lda function with default priors to separate the Species using all other variables
# b) Obtain the prediction vs. actual matrix
# c) Predict the species of a new observation with the following characteristics:
# Sepal.Length = 7.7, Sepal.Width = 3.2, Petal.Length = 5.1, Petal.Width = 2.4
# d) Repeat with qda






library(ggplot2)
set.seed(1)
number_easy = 20
normal_sample <- c()
while (number_easy > 0 & length(normal_sample)<40) {
  s <- truncnorm::rtruncnorm(n = 1,a=0,b=1,mean=0.5,sd=0.5)
  if (s >= 0.6){
    normal_sample <- c(normal_sample,s)
    number_easy <- number_easy - 1
  }
  normal_sample <- c(normal_sample,s)
}
dat <- data.frame(pvalue=normal_sample)
ggplot(dat,aes(x=pvalue))+
  geom_histogram(bins = 10) +
  #geom_density()
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  xlim(0,1)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_minimal()+
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.text =element_text(size=20),
        legend.title =element_text(size=20),
        legend.key.size = unit(1, "cm"))+
  labs(x ="P Value", y = "Density")  

Q <- c()
for (i in normal_sample){
  if (i <= 0.2){
    Q <- c(Q,1)
  }
  else if (i <= 0.4){
    Q <- c(Q,2)
  }
  else if (i <= 0.6){
    Q <- c(Q,3)
  }
  else if (i <= 0.8){
    Q <- c(Q,4)
  }
  else{
    Q <- c(Q,5)
  }
}







set.seed(100)
normal_sample <- truncnorm::rtruncnorm(n = 40,a=0,b=1,mean=0.5,sd=0.5)
dat <- data.frame(pvalue=normal_sample)
ggplot(dat,aes(x=pvalue))+
  #geom_histogram(bins = 10) +
  geom_density() + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  xlim(0,1)+
  scale_x_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme_minimal()+
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.text =element_text(size=20),
        legend.title =element_text(size=20),
        legend.key.size = unit(1, "cm"))+
  labs(x ="P Value", y = "Density")  

Q <- c()
for (i in normal_sample){
  if (i <= 0.2){
    Q <- c(Q,1)
  }
  else if (i <= 0.4){
    Q <- c(Q,2)
  }
  else if (i <= 0.6){
    Q <- c(Q,3)
  }
  else if (i <= 0.8){
    Q <- c(Q,4)
  }
  else{
    Q <- c(Q,5)
  }
}
