
# using smacof in R:

install.packages("MASS")
install.packages("gtools")
install.packages("smacof")
library(MASS)
library(gtools)
library(smacof)
mds(nat.dis,method="manhattan")


library(MASS)   # contains the nMDS routine "isoMDS"nat.
# this step converts sim to dissimilarities, also convert "data frame" to a matrix
nat.dis = 6 - as.matrix(nat.sim)
# make sure the diagonal entries = 0
for (i in 1:5) { nat.dis[i,i]=0 }
nat.dis
# to evaluate stress as a function of dimensionality, you would need to set k=1,2,3,4
# here is the 2-dim solution (k=2)
nat.nmds <- isoMDS(nat.dis,k=2,trace=TRUE,p=2)
nat.nmds

plot(nat.nmds$points)
names = c("Br", "Co", "Cu", "Eg", "Fr")
text(nat.nmds$points,names)



setwd("C:/Users/corter/Documents/TC/hudm5124/0 Spring 2018/Session 6 - weighted MDS")

kinship <- read.delim("C:/Users/corter/Documents/TC/hudm5124/0 Spring 2018/Session 6 - weighted MDS/KINSHIP2_all.txt", sep=" ", 
                        header=TRUE)
kinship
                           
kin1<-as.matrix(kinship[1:15,])
row.names(kin1) = c("AUNT","BROTHR","COUSIN","DAUGHT","FATHER","GRDAUG","GRFATH","GRMOTH","GRSON","MOTHER",
                    "NEPHEW","NIECE","SISTER","SON","UNCLE")
row.names(kin1) = col.names(kinship)

str(kinship)


kin1<-as.dist(kinship[1:15,])
kin1
k1 <- isoMDS(min1,k=2,trace=TRUE,p=2)

kin2<-as.matrix(kinship[16:30,])
row.names(kin2) = c("AUNT","BROTHR","COUSIN","DAUGHT","FATHER","GRDAUG","GRFATH","GRMOTH","GRSON","MOTHER",
                    "NEPHEW","NIECE","SISTER","SON","UNCLE")
kin2<-as.dist(kinship[1:15,])


 
# unfolding examples
beverage <- read.delim("C:/Users/corter/Documents/TC/hudm5124/0 Spring 2018/session 7 - unfolding/ex_rect_data.txt", sep=" ", 
                      header=TRUE)
beverage
dim(beverage)
bev <- smacofRect(beverage[,2:6])
bev
plot(bev, xlim = c(-3, 3), joint = TRUE, asp = 1)


