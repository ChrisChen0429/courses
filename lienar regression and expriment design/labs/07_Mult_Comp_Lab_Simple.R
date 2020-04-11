# Load dat1 and dat2

library(car)
dat1 <- read.csv(file = "C:/Users/Kash/Documents/2019 Fall/HUDM 5123 - TA/Multiple Comparisons Lab/07_dat1.csv")
dat2 <- read.csv(file = "C:/Users/Kash/Documents/2019 Fall/HUDM 5123 - TA/Multiple Comparisons Lab/07_dat2.csv")


# For this lab on error rate control procedures for use
# with multiple comparisons, we use multiple comparison
# procedures with contrast tests from ANOVA analyses.

# For dat1, suppose the primary research question deals with
# differences in prestige due to profession type. Thus, all
# three pairwise comparisons are planned a priori between
# groups: prof vs wc, prof vs bc, and wc vs bc. Because they 
# are planned and consist of all pairwise comparisons, we 
# will use Shaffer's planned post-omnibus procedure. Tukey's 
# procedure would be a second choice.

# Examine data frame
head(dat1)

### Analysis with dat1 (simulated Prestige data set)
dat1$type <- factor(dat1$type, levels = c("prof", "wc", "bc"))
dat1$incomeF <- factor(dat1$incomeF, levels = c("low", "high"))
interaction.plot(x.factor = dat1$type,        # factor on horiz
                 trace.factor = dat1$incomeF, # different lines
                 response = dat1$prestige,    # outcome
                 type = "o",                  # plots connected points
                 xlab = "Type of Profession", # x-axis label
                 ylab = "Prestige",           # y-axis label
                 col = c("black", "green"),   # line colors
                 pch = c(16,17),              # point shapes
                 legend = FALSE)              # no legend
legend(x = "topright",                  # legend goes top right 
       lty = 1, pch = 16:17,            # solid line, point shapes
       col = c("black", "green"),       # colors
       legend = c("Low Income", "High Income")) # legend text

####################################################################################

# Set contrasts for effect coding
options(contrasts = c("contr.sum", "contr.poly"))

lm1 <- lm(prestige ~ type + incomeF + type:incomeF, data = dat1)
summary(lm1)

# The first step in using Shaffers planned post-omnibus
# procedure is to test the omnibus null hypothesis. In this
# case, since this is a two-way ANOVA, we need to check if
# the interaction is signficant before we carry on. 
Anova(lm1, type = "III")

# Two-way interaction not significant. Type and income are.
# Follow up with pairwise comparisons of type averaged over
# income levels.

#############################################################################

library(emmeans)
emm1 <- emmeans(object = lm1,     # full model
                specs = ~ type,   # marginal means for type,
                adjust = "none")  # averaging over income.
emm1

plot(emm1, 
     horizontal = FALSE,
     ylab = "Prestige", 
     xlab = "Type",
     adjust = "none",
     int.adjust = "none",
     comparisons = TRUE)

pairs(emm1, adjust = "none")
# Since the omnibus test for the main effect of type was
# significant, we follow up by ordering the p-values from
# smallest to largest.

# Thus, we find strong evidence that mean prestige score differs
# across all three profession types

###########################################################################

# Some other options for adjustments that work in the
# emmeans package:
pairs(emm1, adjust = "none")
pairs(emm1, adjust = "tukey")
pairs(emm1, adjust = "bonferroni")
pairs(emm1, adjust = "holm")
pairs(emm1, adjust = "scheffe")
pairs(emm1, adjust = "fdr")

plot(emm1, horizontal = FALSE,
     ylab = "Prestige", xlab = "Type",
     adjust = "none",
     int.adjust = "none",
     comparisons = TRUE)
plot(emm1, horizontal = FALSE,
     ylab = "Prestige", xlab = "Type",
     adjust = "tukey",
     int.adjust = "tukey",
     comparisons = TRUE)

# In the plots with "comparisons = TRUE", red arrows are produced
# such that any means that do not overlap with another

#####################################################################
### Analysis with dat2 (real Prestige data set) #####################
#####################################################################
dat2$type <- factor(dat2$type, levels = c("prof", "wc", "bc"))
dat2$incomeF <- factor(dat2$incomeF, levels = c("low", "high"))
interaction.plot(x.factor = dat2$type,
                 trace.factor = dat2$incomeF,
                 response = dat2$prestige,
                 type = "o",
                 xlab = "Type of Profession",
                 ylab = "Prestige",
                 col = c("black", "green"),
                 pch = c(16,17),
                 legend = FALSE)
legend(x = "topright", lty = 1, pch = 16:17,
       col = c("black", "green"), 
       legend = c("Low Income", "High Income"))

###########################################################################

lm2 <- lm(prestige ~ type + incomeF + type:incomeF, data = dat2)
summary(lm2)
Anova(lm2, type = "III")

# Two-way interaction is significant, so must
# look at simple effects.
joint_tests(object = lm2, by = "incomeF")

emm4 <- emmeans(object = lm2,
                specs = ~ type | incomeF,
                adjust = "none")
plot(emm4, 
     horizontal = FALSE,
     ylab = "Prestige", 
     xlab = "Type")
pairs(emm4, adjust = "none")
pairs(emm4, adjust = "tukey")


summary(emm4, adjust = "none")
summary(emm4, adjust = "tukey")

plot(emm4,
     horizontal = FALSE,
     ylab = "Prestige", 
     xlab = "Type",
     adjust = "bonferroni",
     int.adjust = "bonferroni",
     comparisons = TRUE)

