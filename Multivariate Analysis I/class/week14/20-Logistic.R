# Get data from Table 11.2
salmon = read.table("T11-2.DAT")
colnames(salmon) = c("Population", "Gender", "Freshwater", "Marine")
# Population 1 is Alaskan, 2 is Canadian
# Gender 1 is female, 2 is male

# First we need to clean the data. The glm function acceptes only 0 and 1 for y
# We want Alaskan to be 1 and Canadatian 0
salmon$Population = -(salmon$Population-2)
salmon

# Fitting logistic regression of Population on all other variables
logit1 = glm(Population ~ ., data = salmon, family = "binomial")
summary(logit1)

# Predicted probabilities:
predict(logit1, type = "response") 


# Error rate:
y.pred = predict(logit1, type = "response") > 0.5
sum(abs(salmon$Population-y.pred))/length(salmon$Population)
# 7% error rate

# Chi-square test for significance using deviance
with(logit1, null.deviance - deviance)

with(logit1, df.null - df.residual)


# p-value
with(logit1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


# Conclusion: Reject H0: beta1 = beta2 = beta3 = 0, 
# that is, model with predictors is better than intercept only


# Notice gender is not significant, so we can remove it:
logit2 = glm(Population ~ . -Gender, data = salmon, family = "binomial")
summary(logit2)

# Error rate
# Error rate:
y.pred = predict(logit2, type = "response") > 0.5
sum(abs(salmon$Population-y.pred))/length(salmon$Population)
# Still 7% error rate!!

# We can now compare to discriminant analysis:
library(car)
scatterplot(Marine ~ Freshwater | Population, data = salmon, regLine = FALSE, smooth = FALSE)

library(MASS)
ldamod1 = lda(Population ~ . -Gender, data= salmon)



predict(ldamod1)
table(Predicted=predict(ldamod1)$class, Actual = salmon$Population)
# Error rate:
mean(predict(ldamod1)$class != salmon$Population)
# Same error rate but with different false positives and false negatives

# A more realisti way to estimate the error rates is with cross-validation 
# Split the data into training and test sets

set.seed(419)
n = nrow(salmon)
ind <- sample(1:n, 0.8*n)
train <- salmon[ind,]
test <- salmon[-ind,]

# Build the model on the training dataset:
ldamod2 = lda(Population ~ . -Gender, data= train)

# Predict on the test dataset:
table(Predicted = predict(ldamod2, test)$class, Actual = test$Population)
# Error rate:
mean(predict(ldamod2, test)$class != test$Population)
# Now it is 10%

# With logistic regression:
logit3 = glm(Population ~ . -Gender, data = train, family = "binomial")


y.pred = predict(logit3, test, type = "response") > 0.5
sum(abs(test$Population-y.pred))/length(salmon$Population)
# We were very lucky that all test observations were correctly classified!

# Note that if you don't set the seed the answer is different every time!
# To obtain a more stable (less variable) estimate of the true error rate
# We can apply the leave-one-out holdout method

# Vector to store predictions:
pred.holdout = rep(0, n)

for (i in 1:n)
{
  # Remove one observation at a time:
  test = salmon[i,]
  # The rest are training set
  train = salmon[-i,]
  ldamod3 = lda(Population ~ . -Gender, data= train)
  pred.holdout[i] = predict(ldamod3, test)$posterior[2] > 0.5
 }

sum(abs(salmon$Population-pred.holdout))/length(salmon$Population)
# Still 7%

# Now with logistic:
pred.holdout = rep(0, n)

for (i in 1:n)
{
  # Remove one observation at a time:
  test = salmon[i,]
  # The rest are training set
  train = salmon[-i,]
  logit4 = glm(Population ~ . -Gender, data = train, family = "binomial")
  pred.holdout[i] = predict(logit4, test, type = "response") > 0.5
}

sum(abs(salmon$Population-pred.holdout))/length(salmon$Population)
# Again 7%

## Example 2:
# Admission to graduate school based on undergrad GPA, GRE score and undergrad university rank

mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)

summary(mydata)
# Note that rank is technically a categorical predictor but we will ignore that
xtabs(~admit + rank, data = mydata)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)
# Interpretation: 
# For every one point increase in gre, 
# the log odds of admission (versus non-admission) increases by 0.0023
# For every one point increase in GPA, 
# the log odds of admission (versus non-admission) increases by 0.777
# Or better switch to exponent and mulptiplicative change in y:
exp(coef(mylogit))
# For every 1 point extra GRE score the odds of admission increase by 0.23%
# For every 1 point extra GPA  the odds of admission increase by 117%

# CI
confint(mylogit)
# All predictors are significant

# Predictions:

y.pred = predict(mylogit, type = "response") > 0.5
sum(abs(mydata$admit-y.pred))/length(mydata$admit)

# Discriminant
ldamod3 = lda(admit ~ ., data= mydata)


table(Predicted=predict(ldamod3)$class, Actual = mydata$admit)
# Error rate:
mean(predict(ldamod3)$class !=  mydata$admit)

# Leave one out
# Vector to store predictions:
n = nrow(mydata)
pred.holdout = rep(0, n)

for (i in 1:n)
{
  # Remove one observation at a time:
  test = mydata[i,]
  # The rest are training set
  train = mydata[-i,]
  ldamod3 = lda(admit ~ ., data= train)
  pred.holdout[i] = predict(ldamod3, test)$posterior[2] > 0.5
}

sum(abs(mydata$admit-pred.holdout))/n


# Now with logistic:
pred.holdout = rep(0, n)

for (i in 1:n)
{
  # Remove one observation at a time:
  test = mydata[i,]
  # The rest are training set
  train = mydata[-i,]
  logit4 = glm(admit ~ . , data = train, family = "binomial")
  pred.holdout[i] = predict(logit4, test, type = "response") > 0.5
}

sum(abs(mydata$admit-pred.holdout))/n


## Classification trees
library(rpart)
# grow tree
fit <- rpart(admit ~ ., method="class", data=mydata)

fit # display the results 
# What does it mean?
# Without using any predictors, we classify all students as "Deny" since it is 68.25% of cases

plot(fit, uniform=TRUE,
     main="Classification Tree for Admission")
text(fit, use.n=TRUE, all=TRUE, cex=.7)

# Best prediction with one branching is GPA with cutoff of 3.415
# GPA < 3.415 is deny (208 total of which 45 deny)
# GPA > 3.415 is also deny (192 total of which 82 deny)
# However this split helps the brabches further down the tree

# Predictions
conf.matrix = table(mydata$admit, predict(fit,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

# Error rate:
(sum(conf.matrix) - sum(diag(conf.matrix)))/nrow(mydata)
# Slightly better than logistic and discriminant
