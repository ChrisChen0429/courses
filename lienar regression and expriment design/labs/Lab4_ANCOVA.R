# ANCOVA Lab - Acupuncture
# Setting Up
library(car)
library(tidyverse)
# Inputting Data
load("C:/Users/Kash/Documents/2019 Fall/HUDM 5123 - TA/ANCOVA Lab/acupuncture.Rdata")
# Loaded in Acupuncture Data
str(acupuncture)
# There is a problem with factor variables! We should fix them.
acupuncture$sex <- factor(acupuncture$sex, 
                          levels = c(0,1), 
                          labels = c("Male", "Female"))
acupuncture$group <- factor(acupuncture$group, 
                            levels = c(0,1), 
                            labels = c("Ctrl", "Int"))
acupuncture$migraine <- factor(acupuncture$migraine, 
                               levels = c(0,1), 
                               labels = c("Tension", "Migraine"))
str(acupuncture) # checks for new structure.
# We are only going to use the group variable, but I recoded all of them so that
# if we decide to do further analysis, there will not be a problem.

# ANOVA Model - One continuous variable & 1 covariate
# Steps: Run a linear model, check for assumption validation, comment and run
# Anova with the null model. Remember to check for NA values.

lm1 <- lm(pk5 ~ group, 
          data = acupuncture)
summary(lm1)
# We can see here that the group variable is significant, indicating that having
# acupuncture treatment has an effect on the measure of headache severity after
# one year of the treatment (t(1) = -3.44, p < .001). The numerical value of
# -6.097 indicates that having acupuncture treatment will reduce the average
# headache severity rating after 1 year.

boxplot(pk5 ~ group, data = acupuncture) # Interestingly, same as crPlots!
# There doesn't seem to be an issue with the variance ratios.
qqPlot(lm1) # Oh man... We could use a transformation???
influencePlot(lm1) # Really unclear... but should be fine?

lm0 <- lm(pk5 ~ 1,
          data = acupuncture)
summary(lm0) 
# This is only 1 df different, so ok! The reason I was looking at this was to
# make sure that there wasn't any NA issues. If the residual df is more
# different that the number of parameters you are estimating, this would
# indicate the potential for missing values.
anova(lm0, lm1)
# Matches the other analysis of group being significant (F(1,299) = 11.833, p <
# .001)

# Baseline Statistics (d & r)

# Idea is to regress the covariates (or potential ones) along the groups. What
# we want to ensure that there is not preference within each group! We will
# focus primarily on baseline severity (pk1). First we will look at the equations:
# For d, we have d = (mean 1 - mean 2)/pooled sd
by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = mean) # difference
diff(by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = mean)/
       mean(by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = sd))) # standardized

# Manual = 
meand <- diff(by(data = acupuncture$pk1, 
                 INDICES = acupuncture$group, 
                 FUN = mean)) # difference
acuT <- acupuncture[acupuncture$group == "Int",]
acuC <- acupuncture[acupuncture$group == "Ctrl",]
# pooled standard deviation (look at that long equation)
psd <- sqrt(((161-1)*var(acuT$pk1) + (140 - 1)*var(acuC$pk1))/(161+140-2))
# d = 
meand/psd

# r = var ratio
var(acuT$pk1)/var(acuC$pk1)
# No problem, we can move on!

# ANCOVA w/ Baseline Severity

# Ok, to run ANCOVA, we need to run a few preliminary assumptions. The first we
# already did (baseline balance). Now we need to test for a linear relationship.
# We can do this with a correlation or with a linear regression!
cor.test(acupuncture$pk1, acupuncture$pk5) #awesome! 
# This means that pk1 is a good covariate since it is significantly related to
# the outcome. This means that it will account for some linearity in the
# scatterplot.
plot(pk5~pk1, data = acupuncture)
lm2 <- lm(pk5~pk1, data = acupuncture)
summary(lm2)
# pk1 is significantly related to pk5 (t(1) = 17.382, p < .001). We can see that
# a 1 unit increase in the headache severity at baseline has a 0.71 point
# increase in the headache severity at the 1-year measurement.
lm3 <- lm(pk5~group + pk1, data = acupuncture)
summary(lm3)
# Here we can see that even after controlling for pk1, the group is still
# significant. The reason I phrased it this way is because we are interested in
# looking at the effect of the treatment, not the pk1 covariate.
anova(lm2,lm3) # power of GROUP
# This is reinterating the importance of group by showing that adding it to the
# model of lm2 does improve the model's predictive/relative power (F(1,298) =
# 13.427, p < .001).

# ANCOVA w/ Interaction of Severity and Group

# We will now test another assumption of ANCOVA, but also look at the potential
# of a differential effect! We will again create a new model and then compare
# the anova.
lm4 <- lm(pk5 ~ pk1 + group + pk1:group, data = acupuncture)
summary(lm4)
# Oh no, this is a problem for ANCOVA assumptions. We should not have the
# presence of an interaction between the covariate the variable that we are
# focusing on. What a significant interaction implies, in this case, is that the
# way that group and pk5 are related change based on pk1. However, the ANCOVA
# framework is trying to imply that the treatment has the SAME effect regardless
# of pk1 value. This allows for pk1 to be "controlling" for some of the
# variance, rather than having its own effect on how treatment works. While the
# interaction is not problematic for analyses as a whole, the ANCOVA cannot have
# one present.
anova(lm3, lm4)
# Matches! Interaction is significant (F(1,297) = 10.069, p = .002).

# Some Plots
plot(x = acupuncture$pk1, y = acupuncture$pk5)
abline(lm0, lwd = 2, col = "red")

(clr <- as.numeric((acupuncture$group)))
plot(x = acupuncture$pk1, y = acupuncture$pk5,
     xlab = "Baseline Headache Score",
     ylab = "One-Year Headache Score",
     col = c("black", "red")[clr], # black = control, red = treated
     pch = c(16, 17)[clr]) # circles = control, triangles = treated
abline(a = 1.35, b = .69, col = "black", lwd = 2)
abline(a = 1.35 + .96, b = .69 - .13, col = "red", lwd = 2)


plot(pk5 ~ pk1,
     data = acupuncture, 
     xlab = "Baseline Severity Rating (pk1)",
     ylab = "1 Year Severity Rating (pk5)",
     pch = c(19, 0)[as.numeric((acupuncture$group))],
     col = as.numeric((acupuncture$group)),
     main = "ANCOVA Model Assuming No Interaction")
legend(x = "bottomright", 
       lwd = 2, col = 1:2, pch = c(19, 0), lty = 1:2,
       legend = c("Control", "Treatment"), seg.len = 4)

# Two models (by group) based on the ANCOVA model
summary(lm2)
# Control: pk5 = 1.16 + .71*pk1
abline(a = 1.16, b = .71, col = 1, lwd = 2)
# Treated: pk5 = 1.16 - 2.29 + .71*pk1 = -1.13 + .71*pk1
abline(a = -1.13, b = .71, col = 2, lwd = 2, lty = 2)
