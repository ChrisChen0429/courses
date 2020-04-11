
# using smacof for unfolding in R:
#install.packages("smacof")
library(smacof)

################################################################
# Assn 7 unfolding example
setwd("C:/Users/corter/Desktop/HUDM5124")
winter <- read.table("winter olympics unfold.txt",header=TRUE)
winter
dim(winter)

# convert ratings to dissim's
rownames(winter)<-winter[,1]
winter <- 10-winter[,2:13]
winter

wint_om <- smacofRect(winter,type="ordinal",conditionality="matrix")
wint_om
plot(wint_om, xlim = c(-3, 3), asp = 1)

wint_im <- smacofRect(winter,type="interval",conditionality="matrix")
wint_im
plot(wint_im, xlim = c(-3, 3), asp = 1)

wint_or <- smacofRect(winter,type="ordinal",conditionality="row",ndim=2)
wint_or
plot(wint_or, xlim = c(-3, 3), asp = 1)

wint_ir <- smacofRect(winter,type="interval",conditionality="row")
wint_ir
plot(wint_ir, xlim = c(-3, 3), asp = 1)

############# Part 3 ##################
# I choose the row conditional, interval analysis for further exploration
# but the ordinal, matrix conditional solution is also a good one
wint_or1 <- smacofRect(winter,type="interval",conditionality="row",ndim=1,itmax=20000)
wint_or2 <- smacofRect(winter,type="interval",conditionality="row",ndim=2,itmax=20000)
wint_or3 <- smacofRect(winter,type="interval",conditionality="row",ndim=3,itmax=20000)
wint_or4 <- smacofRect(winter,type="interval",conditionality="row",ndim=4,itmax=20000)
wint_or5 <- smacofRect(winter,type="interval",conditionality="row",ndim=5,itmax=20000)

# put the stress values for dim=1 to 5 into an array stressS
stressS<-c(rep(0,5))
stressS[1]<-wint_or1$stress
stressS[2]<-wint_or2$stress
stressS[3]<-wint_or3$stress
stressS[4]<-wint_or4$stress
stressS[5]<-wint_or5$stress
# plot stress for the five dimensionalities
plot(stressS,type="l")
# conclusion: from the stress plot, it looks like 2 dimensions are adequate
# re-check interpretability:
plot(wint_or2, xlim = c(-3, 3), asp = 1)

# final comment:  that fact that several of these runs did not converge 
# suggests that, indeed, the interval matrix-conditional analysis may be best


