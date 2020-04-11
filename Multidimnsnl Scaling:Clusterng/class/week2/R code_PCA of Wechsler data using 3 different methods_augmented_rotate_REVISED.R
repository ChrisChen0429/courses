
# R code to read in wechsler data as a correlation matrix, run a PCA
wech <- read.table("C:/Users/corter/Desktop/HUDM5124/WECHSLER_COR_FULL.csv",sep=",", header=TRUE)
       # or you can use "read.csv" and omit the   sep=","  parameter)
row.names(wech) <- colnames(wech)   ## define row names
wech.cor = as.matrix(wech)   ## convert data table to matrix
wech.cor

#################################################################################################
# NOTE: prcomp will not accept a covariance or correlation matrix as input, so can't use it here.
help(prcomp)


#########################################################################################
# NOTE: "princomp" uses "eigen" as estimation method
# to read in a correlation or covariance matrix, use the "covmat=" option
help(princomp)
wech.cr2 <- princomp(wech, cor = TRUE, scores = TRUE, covmat = wech.cor)
print(wech.cr2)    ## prints out eigenvalues
plot(wech.cr2,type='l')   ## scree plot

P12<-wech.cr2$loadings[,1:2]  ## prepare to plot the first two components, with full variable names
plot(P12,pch=' ')  # plot components with blanks at first
text(P12, rownames(wech.cor))  # then add full subtest names
# So 'princomp' is the simplest way to run PCA on a corr matrix (but below I do PCA via an eigendecomposition).


########################################################################################
# We can also just use "eigen" directly, and define the principal components in terms of 
# the eigendecomposition..
# accomplish a PCA via the eigendecomposition routines in R
# function eigen returns eigenvalues (in $values) and eigenvectors (in $vectors)
help(eigen)

wech.eigen<-eigen(wech.cor,symmetric=TRUE)
wech.eigen
wech.eigen$values
wech.eigen$vectors

# prepare to compute principal components from eigenvectors & eigenvalues
#  first, create 2x2 diagonal identity matrix
I2<-matrix(c(1,0,0,1),nrow=2,byrow=TRUE)
# plug sqrt(eigenvalues) into diagonal of identity matrix to create weighting matrix "wgt"
wgt<-I2
for (i in 1:2) {wgt[i,i]<-(wech.eigen$values[i])^.5}
wgt

# weight the first two eigenvectors to obtain (the first 2) principal components
P2<-wech.eigen$vectors[,1:2] %*% wgt

# now plot the first two principal components
rownames(P2)<-rownames(wech.cor)
plot(P2,pch=' ')
text(P2,rownames(P2))  # add full variable names
# add axes, to better interpret the plot as scalar products
arrows(-.45,0,-.8,0)
arrows(-.46,-.4,-.46,.4)


# reflect the first principal component to enhance ease of interpretation
W<-I2
W[1,1]= -1
W

# postmultiply P2 by matrix wgt to reflect Axis 1
P2 <- P2 %*% W
P2

# now re-plot the first two principal components
plot(P2,pch=' ',asp=1)
text(P2,rownames(P2),asp=1)  # add full variable names
arrows(-.1,0,1,0)
arrows(0,-.4,0,.44)
# add interpretations as axis labels
text(1.11,0,"GENERAL")
text(0,.47,"SPATIAL")

# graphical rotation:
# rotate the axes using a 2 x 2 transformation matrix T
# of the form:   T =  { cos(a) sin(a)
#                      -sin(a) cos(a) }
# T rotates a configuration in 2 dim clockwise by angle a
# for example, for  a = .5,  sin(a)=.48,  cos(a)=.88
T<-matrix(c(.88,.48,-.48,.88),nrow=2,byrow=TRUE)
T
P2T<-P2 %*% T
P2T  # P2T is the rotated component loadings matrix
plot(P2T,pch=' ',asp=1)
text(P2T,rownames(P2),asp=1)  # add full variable names
arrows(-.1,0,.7,0)
arrows(0,-.1,0,.6)


# most common method for orthogonal analytic rotation: VARIMAX
vmax<-varimax(P2,normalize=TRUE,eps=1e-5)
P2_vmax<-vmax$loadings
plot(P2_vmax,pch=' ',asp=1)
text(P2_vmax,rownames(P2),asp=1)  # add full variable names
arrows(-.1,0,.7,0)
arrows(0,-.1,0,.7)
# comment better simple structure, but not perfect (positive corr between components?)


# common methods for oblique rotation:  Oblimin, promax
# try promax here 
pmax<-promax(P2,m=4)
P2_pmax<-vmax$loadings
P2_pmax
plot(P2_pmax,pch=' ',asp=1)
text(P2_pmax,rownames(P2),asp=1)  # add full variable names
arrows(-.1,0,.7,0)
arrows(0,-.1,0,.7)
# comment -- promax also does not seem to give good "simple structure" here


# oblimin oblique analytic rotation -- this gives much better simple structure
install.packages('GPArotation')
library(GPArotation)
P2o<-oblimin(P2)
P2o
P2o$loadings
plot(P2o$loadings,pch=' ',asp=1)
text(P2o$loadings,rownames(P2),asp=1)  # add full variable names
arrows(-.1,0,.7,0)
arrows(0,-.1,0,.7)
# note matrix phi is the "component intercorrelation matrix"


# procrustes rotation: rotate to best fit to a target matrix
# define target matrix F
# NOTE: must install package "smacof" to use this function
F<-matrix(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1),nrow=12,byrow=TRUE)
procrustes(F, P2, scale = TRUE, symmetric = FALSE)



