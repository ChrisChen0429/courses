library(car) # for qqPlot()
library(MASS) # for negative binomial model fitting
library(pscl) # for zero-inflated model fitting
library(lmtest) # for Likelihood ratio tests of glm models
library(AER) # for overdispersion test

load(file = file.choose())
dim(dat)
head(dat)
names(dat)
str(dat)
dat_full <- dat

# Drop rows with missing values
dat <- na.omit(dat)

# Convert STUDY_ARM to a factor. Fill in!
dat$STUDY_ARM <- factor(dat$STUDY_ARM)

# How many control and treated cases?
table(dat$STUDY_ARM)

# Distribution of ages at enrollment.
table(dat$AGE_YRS.1)

# Parent have college degree?
table(dat$EDUC2.1)

# Child's grades in school.
table(dat$GRADES.1)

# Pretest cigarette usage, also by group. Fill in histograms and table by.

table(dat$THIRTYDAYCIG.1)
hist(dat$THIRTYDAYCIG.1)

by(dat$THIRTYDAYCIG.1, dat$STUDY_ARM, mean, na.rm = TRUE)

# Posttest cigarette usage, also by group.

table(dat$THIRTYDAYCIG.2)

by(dat$THIRTYDAYCIG.2, dat$STUDY_ARM, mean, na.rm = TRUE)

# Examine missing data patterns
install.packages("mice",dependencies = T)
library(mice)
?md.pattern
?boys
md.pattern(x = boys)

# Do it on our data.
md.pattern(x = dat_full)
# Reading the left margin:
#  680 cases with no missingness
#  42 cases missing only EDUC2.1
#  15 cases missing only THIRTYDAYCIG.3, etc.
# Reading the bottom margin:
#  47 total cases missing EDUC2.1
#  25 total cases missing THIRTYDAYCIG.3, etc.

# Check balance at baseline (i.e., was randomization successful?)






# Use chi-square test of independence for categorical 
# baseline variables (and Fisher's exact test if 
# expected counts are too low).
chisq.test(table(dat$STUDY_ARM, dat$AGE_YRS.1))
fisher.test(table(dat$STUDY_ARM, dat$AGE_YRS.1))

chisq.test(table(dat$STUDY_ARM, dat$EDUC2.1))

chisq.test(table(dat$STUDY_ARM, dat$GRADES.1))
fisher.test(table(dat$STUDY_ARM, dat$GRADES.1))

# Use t-test (Welch's version, not assuming equal
# group variances) for quantitative baseline variables.
control = dat$THIRTYDAYCIG.1[dat$STUDY_ARM == 0]
interv = dat$THIRTYDAYCIG.1[dat$STUDY_ARM == 1]
t.test(x = control,
       y = interv,
       alternative = "two.sided",
       var.equal = FALSE)

# Estimate the treatment effect with ANOVA. Fill in!


options(contrasts = c("contr.treatment", "contr.poly"))

summary(lm1)
qqPlot(lm1)

cor(dat$THIRTYDAYCIG.1, dat$THIRTYDAYCIG.2, use = "complete.obs")

# Estimate the treatment effect with ANCOVA,
# controlling for baseline
lm2 <- lm(dat$THIRTYDAYCIG.2~dat$STUDY_ARM)
summary(lm2)
qqPlot(lm2)

cor(lm2$fitted.values, dat$THIRTYDAYCIG.2)
plot(lm2$fitted.values, lm2$residuals)
scatter.smooth(x = lm2$fitted.values, y = lm2$residuals)
qqPlot(lm2)
sum(lm2$residuals^2)

# Poisson model with no covariates. Fill in!



summary(glm1)

# Coefficient Interpretation


# Poisson model assumes mean = var
mean(dat$THIRTYDAYCIG.3, na.rm = TRUE)
var(dat$THIRTYDAYCIG.3, na.rm = TRUE)

# Test for overdispersion
dispersiontest(glm1)
# Signifcant result confirms overdispersion

# Poisson model with baseline covariate. Fill in!
glm2 <- glm()
  
  
summary(glm2)

# negative binomial model with no covariates.
nb1 <- glm.nb(THIRTYDAYCIG.2 ~ STUDY_ARM,  
              na.action = na.omit, data = dat)
summary(nb1)

# Does the negative binomial model fit significantly better
# as measured by likelihood-ratio test? NOTE: this test will
# not run unless you delete missing data so that all models
# are fit to exactly the same cases.
lrtest(glm1, nb1)
# Significant result indicates the negative binomial
# model provides a better fit. That is, the additional
# parameter used in the neg bin model is worth it in 
# terms of attaining a better fit to the data.

# negative binomial model with baseline cigarette useage

summary(nb2)

# zero inflated negative binomial

summary(zinb2)

### Define aic & bic functions for use with ZINB models
bic <- function(fit){
  pars <- fit$df.null - fit$df.residual + 2
  n <- fit$n
  lgLik <- logLik(fit)[1]
  out <- -2 * lgLik + log(n) * pars
  return (out)
}

aic <- function(fit){
  pars <- fit$df.null - fit$df.residual + 2
  n <- fit$n
  lgLik <- logLik(fit)[1]
  out <- -2 * lgLik + 2 * pars
  return (pars)
}

c(AIC(lm2), AIC(glm2), AIC(nb2), aic(zinb2))
c(BIC(lm2), BIC(glm2), BIC(nb2), bic(zinb2))

# Both AIC and BIC agree that the zero-inflated model
# is the most parsimonious model for these data.

# Assignment for lab is to replicate these analyses with
# THIRTYDAYCIG.3. Use evidence to determine
# (a) the most parsimonious model for assessing the effect
#     of treatment condition on smoking at the second 
#     follow-up period (THIRTYDAYCIG.3), a year later.
#     Be sure to support your answer with results from
#     a test for overdispersion, likelihood ratio tes0t
#     results, and AIC and BIC results. Also, examine the
#     mean and variance of the outcome and comment on 
#     them.

# (b) Write-up your results about the effect of the 
#     treatment on cigarette smoking a year later.
#     Was the treatment effective? On average, what is
#     the effect of the treatment?

lm2 <- lm(THIRTYDAYCIG.3 ~ THIRTYDAYCIG.1 + STUDY_ARM,data = dat)
glm2 <- glm(data = dat,THIRTYDAYCIG.3 ~ THIRTYDAYCIG.1 + STUDY_ARM,family = 'poisson')
nb2 <- glm.nb(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1,  control = glm.control(maxit = 1000),na.action = na.omit, data = dat)
zinb1 <- zeroinfl(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1 , dist = "negbin", na.action = na.omit, data = dat)
zinb2 <- zeroinfl(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1 | THIRTYDAYCIG.1 + AGE_YRS.1 + EDUC2.1 + GRADES.1, dist = "negbin", na.action = na.omit, data = dat)
c(AIC(lm2), AIC(glm2), AIC(nb2), AIC(zinb1),AIC(zinb2))
c(BIC(lm2), BIC(glm2), BIC(nb2), bic(zinb1),bic(zinb2))
