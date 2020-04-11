
# R code to double-check what form of stress isoMDS uses

# attach to MASS library to access "isoMDS" 
library(MASS)
help(isoMDS)

# first, read in the subset of the nations data used in our precious example of calculating stress 
nat5 <- read.table("C:/Users/corter/Desktop/HUDM5124/nation5.csv",sep=",",header=TRUE)
# use first columnb of crimes as rownames
m<-ncol(nat5)
n5 <- as.matrix(nat5[,2:m])
n5
rownames(n5) <- nat5[,1]
m <- ncol(n5)
n5

S = n5 + t(n5) # symmetrize the matrix n5 by adding it to its transpose --> S
S
maxS <- max(S)  # convert to dissimilarities by subtracting from (largest sim + 1)
maxS
D <- (maxS+1)-S
D
for (i in 1:m) { D[i,i]<-0 }   # set diagonal entries to 0
D

# specify an initial configuration matrix
X <- matrix(c(1,0,0,1,-1,0,0,-1,0,0),byrow=TRUE,nrow=5)
X

nat5.sol<- isoMDS(D,y=X,k=2,trace=TRUE,p=2,maxit=1)
stress<-nat5.sol$stress
stress

# comparing the initial value of stress to the value we obtained for Assn 4C (calculating stress),
# it can be seen that isoMDS reports the value of Stress1 (the SQRT version) x 100 
# (i.e reported as a %)

