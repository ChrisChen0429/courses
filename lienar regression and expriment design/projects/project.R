library(car) # for qqPlot()
library(MASS) # for negative binomial model fitting
library(pscl) # for zero-inflated model fitting
library(lmtest) # for Likelihood ratio tests of glm models
library(AER) # for overdispersion test
library(dplyr)
library(multcomp)
library(ggplot2)

# please check the file address 
setwd("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/projects")

# read the data
data <- read.table('ceb.dat')
# set up the ordinal variables
data$educ <- factor(data$educ,levels = c('none','lower','upper','sec+'))
data$dur <- factor(data$dur,levels = c('0-4','5-9','10-14','15-19','20-24','25-29'))
data$res <- factor(data$res,levels = c('Suva','urban','rural'))
# exploret the data
head(data)
summary(data)


# one-way ANOVA
lm1 <- lm(mean ~ educ+res, data = data)
Anova(lm1)
lm1
plot(data$educ,data$mean, xlab="education", ylab="children ever born")
data %>% group_by(educ) %>% summarise(sd(mean))


# two way ANOVA
lm2 <- lm(mean ~ educ+dur, data = data)
summary(lm2)
Anova(lm2, type = 3)

# interaction plot
data %>%
  ggplot() +
  aes(x = educ, color = dur, group = dur, y = y) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

# adding interaction
lm2 <- lm(mean ~ educ*dur, data = data)
summary(lm2)
Anova(lm2, type = 3)


## linear constrast
# situation one
lm3 <- lm(data = data,mean ~ res)
c1 <- matrix(c(1,-0.5,-0.5), 1)
t1 <- glht(lm3, linfct = c1)
summary(t1)
# situation two 
lm4 <- lm(data = data,mean ~ educ)
c2 <- matrix(c(0,1,1,0), 1)
t2 <- glht(lm4, linfct = c2)
summary(t2)


## loglinearmodels
data$logn = log(data$n)
data$newy = log(data$y) - data$logn
lm0 <- lm(newy~1, data)
lm1 <- lm(newy~dur,data)
lm2 <- lm(newy~res,data)
lm3 <- lm(newy~educ,data)
lm4 <- lm(newy~dur+res,data)
lm5 <- lm(newy~dur+educ,data)
lm6 <- lm(newy~res+educ,data)
lm7 <- lm(newy~res+educ+dur,data)
c(AIC(lm0),AIC(lm1),AIC(lm2),AIC(lm3),AIC(lm4),AIC(lm5),AIC(lm6),AIC(lm7))
c(BIC(lm0),BIC(lm1),BIC(lm2),BIC(lm3),BIC(lm4),BIC(lm5),BIC(lm6),BIC(lm7))
summary(lm7)




