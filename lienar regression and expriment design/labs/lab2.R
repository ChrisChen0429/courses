load("~/Desktop/yi/professional_study/courses/lienar regression and expriment design/labs/ecls2.Rdata")
library(car)
data <- Prestige
data <- data[complete.cases(data), ]
lm_F1 <- lm(prestige ~ factor(type),data)
summary(lm_F1)
lm_R1 <- lm(prestige ~1,data)
anova(lm_R1,lm_F1)


contrasts(data$type) = contr.sum(3)
attributes(data$type)$contrasts['bc',] <- c(-1,-1)
attributes(data$type)$contrasts['wc',] <- c(1,0)
lm_F1 <- lm(prestige ~ type,data)
summary(lm_F1)
lm_R1 <- lm(prestige ~1,data)
anova(lm_R1,lm_F1)

