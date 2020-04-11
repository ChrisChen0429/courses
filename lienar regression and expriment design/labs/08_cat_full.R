# Lab 08 - Non-Continuous Outcomes

# Including in the Data
load(file.choose())
# Exploring the Data
head(ecls2)
str(ecls2)
table(ecls2$PROF5)
table(ecls2$SPED)
table(ecls2$SPED, ecls2$PROF5)

# Boxplots for Diagnostics
boxplot(ecls2$MATHK ~ ecls2$PROF5,
        horizontal = TRUE,
        main = "Boxplots of Baseline Proficiency by 5th Grade Proficiency",
        xlab = "Kindergarten Math Score",
        ylab = "5th Grade Proficiency Status")

# boxplot(ecls2$MATHK ~ ecls2$PROF5,
#         horizontal = TRUE,
#         axes = FALSE)
# axis(side = 1, at = seq(0, 100, 20))
# axis(side = 2, at = 1:2, labels = c("Not Proficient", "Proficient"))
# title(main = "Boxplots of Baseline Proficiency by 5th Grade Proficiency",
#       xlab = "5th Grade Proficiency Status")

boxplot(ecls2$MATHK ~ ecls2$SPED,
        horizontal = TRUE,
        main = "Boxplots of Baseline Proficiency by Special Education Status",
        xlab = "Kindergarten Math Score",
        ylab = "Special Education Status")

# Scatteplot with Jitter
plot(x = ecls2$MATHK,
     y = jitter(ecls2$PROF5),
     xlab = "Baseline Math Score",
     ylab = "5th Grade Proficiency",
     main = "Scatterplot of Baseline Proficiency by KG Math Score with Vertical Jitter",
     axes = FALSE,
     xlim = c(0, 100),
     ylim = c(-0.4, 1.2))
axis(side = 1, at = seq(0, 100, 20))
axis(side = 2, at = 0:1, labels = c("0 = Not Proficient", "1 = Proficient"))
abline(h = 0:1, col = "gray", lty = 2)

# Linear Regression
lm1 <- lm(PROF5 ~ MATHK, data = ecls2)
summary(lm1)
abline(lm1, col = "red", lwd = 3)

# Adding in SPED Variable
lm2 <- lm(PROF5 ~ MATHK + SPED, data = ecls2)
summary(lm2)
