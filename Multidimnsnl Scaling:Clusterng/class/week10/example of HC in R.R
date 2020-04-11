
help(hclust)   # one of many clustering packages in R

# set working directory
setwd("C:/Users/corter/Desktop/HUDM5124")

# read in similarity matrix (% cognates among languages; Rea, 1958)
lang <- read.csv("LANGROM1_PRX_R.txt",sep=" ",header=TRUE)
lang
# plug 0's for missing values in diagonal and upperhalf of proximity matrix
n<-ncol(lang)
for (i in 1:n) {
  for (j in 1:n) { 
  lang[i,j]<-ifelse(is.na(lang[i,j]),0,lang[i,j])
  } }
# symmetrize matrix
langsym <- lang+t(lang)
# convert to dissimilarities
langsym <- 100-langsym
# re-set diagonal entries to 0
for (i in 1:n) { langsym[i,i]=0 }
langsym

langrom<-as.dist(langsym)

# Hierarchical Agglomerative Methods -- average method, 
#  this method a.k.a. "UPGMA" = "unweighted pair-groups method using averages
fit <- hclust(langrom, method="average") 
plot(fit) # display dendogram

# Hierarchical Agglomerative Methods -- Ward's method (using squared distances)
fit <- hclust(langrom, method="ward.D2") 
plot(fit) # display dendogram


###################################################################################
# NOTE: if you start with a multivariate matrix X, first you need to compute distances
help(dist)

# a data matrix (note one missing value, also column 3 is a multiple of column 1)
X <- matrix(c(1,0,2,3,4,6,1,7,2,4,2,8,3,4,6,NA,3,6),byrow=TRUE,ncol=3)
colnames(X) <- c("X1","X2","X3")
rownames(X) <- c("s1","s2","s3","s4","s5","s6")
X

# some useful functions
goodX <- na.omit(X)     # delete cases with missing values (listwise)
goodX
normedX <- scale(goodX)   # standardize variables (not necessarily advised)
normedX

# compute Euclidean distances among cases (rows)
D <- dist(normedX,method="euclidean")
D



