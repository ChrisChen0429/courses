
# smacof weighted-MDS of new classroom demo nations data (by indiv subject)

# if not yet done, install smacof package
install.packages("smacof")
# attach to the library
library(smacof)
# documentation:
help(smacofIndDiff)


setwd("C:/Users/corter/Desktop/HUDM5124")
# first, read in the new nations data (ALL RATERS -- 2019 & 2020 ratings)
nnatALL <- read.table("nations_ALLDATA_1920.csv",sep=",",header=TRUE)
nnatALL
# extract rectangular matrix of actual ratings
m <- ncol(nnatALL)
m
nnA<-nnatALL[1:66,4:21]
nnA
m <- ncol(nnA)
m
n <- nrow(nnA)
n
# matrix nnA now consists of ratings of 66 pairs of nations by 18 raters



# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,1]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn1<-as.dist(sim2diss(nn,method=10))
nn1

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,2]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn2<-as.dist(sim2diss(nn,method=10))
nn2

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,3]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn3<-as.dist(sim2diss(nn,method=10))
nn3

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,4]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn4<-as.dist(sim2diss(nn,method=10))
nn4

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,5]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn5<-as.dist(sim2diss(nn,method=10))
nn5

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,6]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn6<-as.dist(sim2diss(nn,method=10))
nn6

natALLm<-list(nn1,nn2,nn3,nn4,nn5,nn6)



# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,7]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn7<-as.dist(sim2diss(nn,method=10))
nn7

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,8]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn8<-as.dist(sim2diss(nn,method=10))
nn8

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,9]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn9<-as.dist(sim2diss(nn,method=10))
nn9

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,10]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn10<-as.dist(sim2diss(nn,method=10))
nn10

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,11]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn11<-as.dist(sim2diss(nn,method=10))
nn11

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,12]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn12<-as.dist(sim2diss(nn,method=10))
nn12


# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,13]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn13<-as.dist(sim2diss(nn,method=10))
nn13

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,14]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn14<-as.dist(sim2diss(nn,method=10))
nn14

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,15]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn15<-as.dist(sim2diss(nn,method=10))
nn15

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,16]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn16<-as.dist(sim2diss(nn,method=10))
nn16

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,17]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn17<-as.dist(sim2diss(nn,method=10))
nn17

# now slice off each column as one rater's ratings for the 66 pairs
# then convert each column to a lowerhalf matrix of ratings
nr1<-nnA[,18]
nr1
# read in this vector as an upperhalf string, convert it to lowerhalf form
m<-12
nn<-matrix(c(rep(0,m*m)),byrow=TRUE,nrow=m)  #initialize an n x n matrix of 0's
ij<-0
for (i in 1:(m-1)) # for each row of upperhalf matrix 
{ for (j in (i+1):m)  # for each column " " "
{ ij <- ij+1
nn[j,i]<-nr1[ij] }}
nn
names<-c("Brazil","Congo","Cuba","Egypt","France","India","Israel","Japan","China","Russia","USA","Serbia")
names
colnames(nn) <- names 
rownames(nn) <- names 
nn
nn18<-as.dist(sim2diss(nn,method=10))
nn18

natALLm18<-list(nn1,nn2,nn3,nn4,nn5,nn6,nn7,nn8,nn9,nn10,nn11,nn12,nn13,nn14,nn15,nn16,nn17,nn18)
str(natALLm18)

# small-data version:
#natALLm<-list(nn1,nn2,nn3,nn4,nn5,nn6)
#natALLm


# try to obtain solutions in 1 to 6 dimensions, put stress values into an array for plotting
stressd<-c(rep(0,6))
nnat_ind1<- indscal(natALLm18,ndim=1,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[1]<-nnat_ind1$stress
nnat_ind2<- indscal(natALLm18,ndim=2,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[2]<-nnat_ind2$stress
nnat_ind3<- indscal(natALLm18,ndim=3,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[3]<-nnat_ind3$stress
nnat_ind4<- indscal(natALLm18,ndim=4,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[4]<-nnat_ind4$stress
nnat_ind5<- indscal(natALLm18,ndim=5,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[5]<-nnat_ind5$stress
nnat_ind6<- indscal(natALLm18,ndim=6,type="ordinal",init="torgerson",verbose=TRUE,itmax=5000)
stressd[6]<-nnat_ind6$stress

plot(stressd[1:6],type="l")

# let's examine the 4D solution for interpretability
# plot the group stimulus space
plot(nnat_ind4$gspace[,1:2],asp=1,pch=' ')
text(nnat_ind4$gspace[,1:2],names)
plot(nnat_ind4$gspace[,3:4],asp=1,pch=' ')
text(nnat_ind4$gspace[,3:4],names)

# let's examine the 3D solution for interpretability
# plot the group stimulus space
plot(nnat_ind3$gspace[,1:2],asp=1,pch=' ')
text(nnat_ind3$gspace[,1:2],names)
plot(nnat_ind3$gspace[,2:3],asp=1,pch=' ')
text(nnat_ind3$gspace[,2:3],names)


# do the same for smacofSym on the averaged nations data
# that data is in matrix Dx, from last week
Dx
nn1.sol<- smacofSym(Dx,ndim=1,type="ordinal",init="torgerson")
nn2.sol<- smacofSym(Dx,ndim=2,type="ordinal",init="torgerson")
nn3.sol<- smacofSym(Dx,ndim=3,type="ordinal",init="torgerson")
nn4.sol<- smacofSym(Dx,ndim=4,type="ordinal",init="torgerson")
nn5.sol<- smacofSym(Dx,ndim=5,type="ordinal",init="torgerson")
nn6.sol<- smacofSym(Dx,ndim=6,type="ordinal",init="torgerson")
# put the stress values for runs=1 to 5 into an array stressDim
stressS<-c(rep(0,6))
stressS[1]<-nn1.sol$stress
stressS[2]<-nn2.sol$stress
stressS[3]<-nn3.sol$stress
stressS[4]<-nn4.sol$stress
stressS[5]<-nn5.sol$stress
stressS[6]<-nn6.sol$stress
# plot stress for the six dimensionalities -- averaged data
plot(stressS,type="l")

# plot stress for the six dimensionalities for both smacofIndDiff and smacofSym
plot(1,type='n',xlim=c(1,6),ylim=c(0,0.3),xlab='ndim', ylab='Stress')
lines(stressd, type='o', lwd=2)
lines(stressS, type='o', lwd=1)


# part 4: plot weight space (I will illustrate for D1 & D2 of the 3-dim solution
nnat_ind3$cweights
wgts<-matrix(c(rep(0,54)),ncol=3)
wgts

for (s in 1:18) {
  w <- nnat_ind3$cweights[s]
  wgts[s,] <- diag(w[[1]])
}
wgts

# plot ID codes for Dims 1 & 2
subjects<-c("DAL","HED","119","909","124","111","FOB","181","JEC","MC15","3356","iwq","a1b2","724","519","2226","4","6590")
plot(wgts[,1:2],asp=1,pch=' ')
text(wgts[,1:2],subjects

# plot birth country for Dims 1 & 2)
birth<-c("USA","CHINA","Korea","USA","China","China","China","China","USA","Singapore","China","China","China","USA","Canada","HONG KONG","Turkey","china")
plot(wgts[,1:2],asp=1,pch=' ')
text(wgts[,1:2],birth)


