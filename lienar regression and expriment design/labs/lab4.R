setwd("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/labs")
load("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/labs/acupuncture.Rdata")

library(ggplot2)
library(dplyr)
interaction.plot(acupuncture$migraine, acupuncture$group, acupuncture$pk5)


acupuncture2$group <- factor(acupuncture2$group)
acupuncture2 %>% 
  ggplot() +
  aes(x = migraine, y = headache, color = group) +
  geom_line(aes(group = group)) +
  geom_point() 


# interaction effects
model1 <- lm(pk5~group*migraine,data=acupuncture)
model1
model2 <- lm(pk5~group+migraine,data=acupuncture)
model2
anova(model2,model1)

model3 <- lm(pk5~group+group:migraine ,data=acupuncture)
anova(model3,model1)
model4 <- lm(pk5~migraine+group:migraine ,data=acupuncture)
anova(model4,model1)


acupuncture %>% group_by(migraine,group) %>% summarise(mean = mean(pk5),standard_deviation = sd(pk5),number = count(pk5)) 


variable = acupuncture$pk5[acupuncture$migraine==0]
group =  acupuncture$group[acupuncture$migraine==0]
x_bar_c <- mean(variable[group==0])
x_bar_t <- mean(variable[group==1])
s_c <- sd(variable[group==0])
s_t <- sd(variable[group==1])
s_pool <- sqrt( (((length(variable[group==0])-1)*(s_c^2)  + (length(variable[group==1])-1)*(s_t^2)))/(length(variable)-2))
d <- (x_bar_t - x_bar_c) / s_pool
t_test <- t.test(variable[group==0],variable[group==1])
t_test


variable = acupuncture$pk5[acupuncture$group==1]
migraine =  acupuncture$migraine[acupuncture$group==1]
x_bar_c <- mean(variable[migraine==0])
x_bar_t <- mean(variable[migraine==1])
s_c <- sd(variable[migraine==0])
s_t <- sd(variable[migraine==1])
s_pool <- sqrt( (((length(variable[migraine==0])-1)*(s_c^2)  + (length(variable[migraine==1])-1)*(s_t^2)))/(length(variable)-2))
d <- (x_bar_t - x_bar_c) / s_pool
d
t_test <- t.test(variable[migraine==0],variable[migraine==1])
t_test
