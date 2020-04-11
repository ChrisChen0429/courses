# rmd1 has 10 subjects and one repeated 
# factor: severity of visual interference.
# The outcome is reaction time to find
# the letter on the screen.
rmd1

# Variance/covariance matrix 
round(var(rmd1), 2)

# Correlation matrix
round(cor(rmd1), 2)

# Last class we assumed compound symmetry
# and used an epsilon-type correction to the
# degrees of freedom for the F test to accont
# for the fact that compound symmetry was 
# violated. Today we will explore allowing
# other, more flexible specifications of the
# variance covariance matrix.

# Load the package nlme.
library(nlme)

# The gls() function may be used to fit general
# linear models. Note the argument called 
# correlation.
?gls

# The corStruct() function is used to specify 
# the correlation structure.
?corStruct

# lme4 can be used to fit random intercept 
# models like the ones we discussed last class.
# Recall that the random intercept model, when
# used with repeated-measures data, implies 
# a compound symmetric variance/covariance 
# matrix for repeated measures (i.e., for each
# subect). Function lmer() is used to fit linear
# mixed effects models like the random intercept
# model.
?lme

# lme requires that data be in 'long' form, with
# one row per observation and a subject ID. Right
# now our data are in 'wide' form, with one row
# per subject and separate columns for repeated
# measures.

# install.packages("tidyr")
library(tidyr)

# The gather() function may be used to go from 
# wide to long. The spread() function goes the
# other way.

# First, add an= subject ID variable to rmd1.
rmd1 <- cbind(id = 1:10, rmd1)
rmd1

rmd1_long <- gather(data = rmd1, key = severity, 
                    value = time, No, Mil, Mod, Sev)
rmd1_long
str(rmd1_long)
rmd1_long$severity <- factor(rmd1_long$severity, ordered = TRUE)
str(rmd1_long)

# Now run the random intercept model.
lme1 <- lme(time ~ severity, 
            data = rmd1_long, 
            random = ~ 1|id,
            method = "REML")
summary(lme1)
AIC(lme1)
BIC(lme1)

# Here is another way to run a random intercept model
# in R; this time using package lme4:
library(lme4)
library(lmerTest)
lmer1 <- lmer(formula = time ~ severity + (1|id), 
              data = rmd1_long, REML = TRUE)
summary(lmer1)

# Next, fit the same model using gls().
gls1 <- gls(model = time ~ severity, data = rmd1_long,
            correlation = corCompSymm(form = ~ 1|id))
summary(gls1)
AIC(gls1)
BIC(gls1)

# Try AR(1) for the var/cov specification.
gls2 <- gls(model = time ~ severity, data = rmd1_long,
            correlation = corAR1(form = ~ 1|id))
summary(gls2)
AIC(gls2)
BIC(gls2)

# Try unstructured for the var/cov specification.
gls3 <- gls(model = time ~ severity, data = rmd1_long,
            correlation = corSymm(form = ~ 1|id))
summary(gls3)
AIC(gls3)
BIC(gls3)


