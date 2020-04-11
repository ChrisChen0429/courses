# Two-Way MANOVA
library(foreign)
hsb = read.spss(file.choose())
hsb


# Suppose we want to test if there is difference in average scores of read, write and math
# But this time we use both program and gender as Factor1 and Factor 2
# Create the list of the p dependent variables:
dependent.vars = cbind(hsb$READ, hsb$WRITE, hsb$MATH)

# One-Way MANOVA as on Test 1
summary(manova(dependent.vars ~ hsb$PROG), test = "Wilks")

# Two-Way MANOVA:
summary(manova(dependent.vars ~ hsb$PROG + hsb$FEMALE), test = "Wilks")
# Conclusion: 
# Since all p-values < 0.05 both factors are significant and affect average performance scores

# Two-Way MANOVA with interaction effect:
manova.int = manova(dependent.vars ~ hsb$PROG * hsb$FEMALE)
summary(manova.int, test = "Wilks")
# Conclusion: Both factors and their interaction are significant

# SS matrices:

library(car)

fit <- lm(dependent.vars ~ PROG*FEMALE, data= hsb)

# Sequential SS
ManRes <- Manova(fit, type="II")
summary(ManRes, multivariate=TRUE)

# Each term entered last SS
ManRes <- Manova(fit, type="III")
summary(ManRes, multivariate=TRUE)
