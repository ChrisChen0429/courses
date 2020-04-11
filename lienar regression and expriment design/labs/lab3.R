setwd("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/labs")
load("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/labs/acupuncture.Rdata")
library(car)
library(ggplot2)

# task 1
boxplot(pk5~group,data=acupuncture, main="Box Plot",xlab="Posttest Severity Rating", ylab="Acupencutre Treatment Group")
full_model <- lm(pk5~group,data=acupuncture)
reduced_model <- lm(pk5~1,data=acupuncture)
anova(reduced_model,full_model)
RegSS =  sum((acupuncture$group * full_model$coefficients[2]  +  full_model$coefficients[1]  - mean(acupuncture$group * full_model$coefficients[2]  +  full_model$coefficients[1] ))^2)
RSS = sum((acupuncture$pk5 - acupuncture$group * full_model$coefficients[2] -  full_model$coefficients[1])^2)

ggplot(acupuncture, aes(x=pk1, y=pk5)) + geom_point(aes(col=factor(group))) + geom_hline(yintercept=full_model$coefficients[1], linetype="dashed", color = "blue", size=2) +
    geom_hline(yintercept=full_model$coefficients[1] + full_model$coefficients[2],  linetype="dashed", color = "red", size=2)


# task 2
continous_baseline_check <- function(variable = acupuncture$age, group = acupuncture$group){
  x_bar_c <- mean(variable[group==0])
  s_c <- sd(variable[group==0])
  x_bar_t <- mean(variable[group==1])
  s_t <- sd(variable[group==1])
  s_pool <- sqrt( ((length(variable[group==0]-1)*s_c  + (length(variable[group==1])-1)*s_t))/(length(variable)-2))
  d <- (x_bar_t - x_bar_c) / s_pool
  r <- (s_t / s_c)^2
  t_test <- t.test(variable[group==0],variable[group==1])
  result <- list("x_bar_c"=x_bar_c,"s_c"=s_c,"x_bar_t"=x_bar_t,"s_t"=s_t,"d"=d,"r"=r,"sig"=t_test$p.value)
  print(result)
}

cat_baseline_check <- function(variable = acupuncture$age, group = acupuncture$group){
  x_bar_c <- mean(variable[group==0])
  s_c <- sd(variable[group==0])
  x_bar_t <- mean(variable[group==1])
  s_t <- sd(variable[group==1])
  s_pool <- sqrt( ((length(variable[group==0]-1)*s_c  + (length(variable[group==1])-1)*s_t))/(length(variable)-2))
  d <- (x_bar_t - x_bar_c) / s_pool
  r <- (s_t / s_c)^2
  chi_test <- chisq.test(variable,group)
  result <- list("x_bar_c"=x_bar_c,"s_c"=s_c,"x_bar_t"=x_bar_t,"s_t"=s_t,"d"=d,"r"=r,"sig"=chi_test$p.value)
  print(result)
}




continous_baseline_check(acupuncture$age)
cat_baseline_check(acupuncture$sex)
cat_baseline_check(acupuncture$migraine)
continous_baseline_check(acupuncture$chronicity)
continous_baseline_check(acupuncture$pk1)
continous_baseline_check(acupuncture$pk5)
