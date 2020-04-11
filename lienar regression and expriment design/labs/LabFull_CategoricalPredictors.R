### Lab 03 - Categorical Predictors
library(car)
library(tidyverse)
library(pander)
# Quick note about pipelines

# The symbol  %>%  allows for feeding one function into another. For example,
# let's say you wanted to calculate the mean of a list of numbers, you could do
# this:
c(2,3,4,5) # creates a vector of values
mean(c(2,3,4,5)) # calculates the mean using "nested" approach
c(2,3,4,5) %>% mean(.) # calculates the mean using pipelines

# Viewing the Data
?Prestige
str(Prestige)

# Relevel the categories
Prestige$type <- factor(Prestige$type, levels = c("bc", "wc", "prof"))
str(Prestige)

# Exploring the data
hist(Prestige$prestige, breaks = 50) 
# This looks generally normally distributed, so we shouldn't have an issue here.
Prestige %>% 
  filter(!is.na(type)) %>%
  group_by(type) %>%
  summarise_all(., list(~var(., na.rm = T)))
# This code allows to look at variances across groups. We can see that for the
# prestige measurement, the variance ratios are not larger than 3. This means
# that the homogeneity of variances is sustained.

# Boxplot of Data
boxplot(Prestige$prestige ~ Prestige$type)
# Matches the variance conclusion above

# Creating dummy variables from factors
Prestige$wc <- ifelse(Prestige$type == "wc", 1, 0)
Prestige$prof <- ifelse(Prestige$type == "prof", 1, 0)
# This above code creates two dummy coded variables.
str(Prestige)
# You can see the 0 and 1. Remember that we do not create 3 variables because it
# would collinear in the model.

# Running a full model
lm_F1 <- lm(prestige ~ factor(wc) + factor(prof), 
            data = Prestige)
lm_R1 <- lm(prestige ~ 1, 
            data = Prestige %>% 
              filter(!is.na(type)))

# Comparing the models in an anova table
anova(lm_R1, lm_F1)
# We can see here that the incremental F test is significant. This means that
# there is a significant relationship between the type variable (coded as
# dummies) and the outcome of prestige, (F(2,95) = 109.59, p < .001). We can
# also say that the type of job does affect the rating of prestige attributed to
# that job.

# Creating Dev. Codes
Prestige$wc_dev <- ifelse(Prestige$type == "bc", -1, Prestige$wc)
Prestige$prof_dev <- ifelse(Prestige$type == "bc", -1, Prestige$prof)
# These lines create deviation coded variables that match the wc and prof dummy
# variables and have -1 for both dummies if the type is "bc". This creates an
# intercept that equates to the population, or grand, mean.

# You can see that we used the %>% pipeline function to filter out the na
# values. Instead of this, we can create a new dataset that removes the NA
# values in the "type" variable. I knew that there were "NA" values because I
# just looked at the dataset!

Prestige_noNA <- Prestige[is.na(Prestige$type) == F,]
# This filters out all the ROWS that are NOT NA values and keeps all the column
# values.

lm_F2 <- lm(prestige ~ factor(wc_dev) + factor(prof_dev), 
            data = Prestige_noNA)
lm_R2 <- lm(prestige ~ 1, 
            data = Prestige_noNA)

# Comparing the models in an anova table
anova(lm_R2, lm_F2)
# We can see here that the incremental F test is significant and matches the
# output from the previous ANOVA. This makes complete sense because you aren't
# changing the data, just changing the way that you CODE the data. The
# relationship should still be the same.


