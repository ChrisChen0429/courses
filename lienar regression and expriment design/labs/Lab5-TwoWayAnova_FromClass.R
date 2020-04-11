load(file.choose())
acupuncture$migraine <- factor(x = acupuncture$migraine,
                               levels = c(0,1))
acupuncture$group <- factor(x = acupuncture$group,
                            levels = c(0,1))
str(acupuncture)

options(contrasts = c("contr.sum", "contr.poly")) 
#changes to deviation coding

# Task 1
lm1 <- lm(pk5 ~ migraine*group, data = acupuncture)
summary(lm1) # look at full model
lm1R <- lm(pk5 ~ group + migraine, data= acupuncture)
summary(lm1R) # look at reduced model
anova(lm1R,lm1) # incremental F test to see interaction term

# Task 2 Simple Effects
interaction.plot(x.factor = acupuncture$group,
                 trace.factor = acupuncture$migraine,
                 response = acupuncture$pk5,
                 ylim = c(0,88)) # looks at interaction plot and rescales to the full range of the outcome
range(acupuncture$pk5) # range of outcome
install.packages("emmeans")
library(emmeans)
em1 <- emmeans(object = lm1,
        specs = ~group|migraine) # marginal means of interaction
# This line looks at the marginal means of the data. This looks at the average
# post severity (pk5) of the control and treatment groups BASED on the level of
# migraine.
pairs(em1)
# This does a pairwise test of each of the pairs that were defined by em1.
# You could use group:migraine to look at a different emmeans.

# Task 3 Main Effects
em2 <- emmeans(object = lm1,
               specs = ~group)
em2
# This looks at the main effect of group, meaning that it is averaging across
# the levels of migraine. Essentially we don't care about what migraine group is
# saying.
pairs(em2)
install.packages("car")
library(car)
Anova(lm1, type = "III")
# This does a type III sum of squares ANOVA for a SINGLE model. Essentially it
# will complete incremental F tests for each variable in the full model. It
# automatically constructs all the reduced models that are needed and runs the
# incremental F tests and gives you the full output. You use this for the ANOVA
# table that you produce for you write-up.


# HW: Change all of these into an APA formatted document. No task headers!




















