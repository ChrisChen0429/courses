
#  fitting ultrametric and additive trees in R

# gather some R documentation:
#??ultrametric
#The search string was "ultrametric"
#Help pages:
#clue:: cl_ultrametric	     Ultrametrics of Hierarchies
#clue:: ls_fit_ultrametric_target	Fit Dissimilarities to a Hierarchy
#clue:: l1_fit_ultrametric    Least Absolute Deviation Fit of Ultrametrics to Dissimilarities
#clue:: ls_fit_sum_of_ultrametrics	Least Squares Fit of Sums of Ultrametrics to Dissimilarities
#clue:: ls_fit_ultrametric	      Least Squares Fit of Ultrametrics to Dissimilarities
--------------------------------------------------------------
#??addtree
#The search string was "addtree"
#Help pages:
#clue::as.cl_addtree		Additive Tree Distances
#clue::ls_fit_addtree		Least Squares Fit of Additive Tree Distances to Dissimilarities
#==============================================================
# install the "clue" package for fitting trees
library(clue)      # for tree-fitting & clustering

help(ls_fit_ultrametric)
help(ls_fit_addtree)

methods(plot)
help(plot.dendrogram)

#=================================================
# now fit an ultrametric tree (LS solution, by iterative projection) to a sample data set
a <- eurodist   # a sample data set = distances among European cities
ultraD<-ls_fit_ultrametric(eurodist,method=c("IP"), weights = 1, control = list())
ultraD  # this prints a matrix of model distances
plot(ultra) # plots the fitted ultrametric tree

# calculate the "cophenetic correlation" - the corr of data & model distances
datvec=as.vector(eurodist)
modvec=as.vector(ultraD)
cor(datvec,modvec)

# just for exploration purposes, let's find & draw the tree structure corresponding 
#  to the hierarchical clustering derived using the average (UPGMA) method
hc2 <- hclust(eurodist, "ave")
(dend2 <- as.dendrogram(hc2)) # "print()" method
plot(dend2)
u <- cl_ultrametric(hc2)  # compute ultrametric distances from tree structure
datvec=as.vector(eurodist)
modvec=as.vector(u)
cor(datvec,modvec)   # note that the "cophenetic correlation" is lower -- the HC solution is not optimized

#=================================================
# now fit an additive tree (LS solution, by "iterative projection" method -- Hubert & Arabie, 1995) to the sample data set
#eurodist   # a sample data set = distances among European cities

addD=ls_fit_addtree(eurodist,method=c("IP"), weights = 1, control = list())
addD
# calculate linear fit = correlation of data & model distances
datvec=as.vector(eurodist)
modvec=as.vector(addD)
cor(datvec,modvec)

#===================================================
# fitting an additive tree by "iterative projection" method -- ref. Hubert & Arabie (1995)
# IP uses a semi-randomized start.  It is susceptible to local minima, so run multiple starts. 
  eurodist   # a sample data set = distances among European cities
  datvec<-as.vector(eurodist)

# run 25 random starts
# save r=cor of data & model distances for each run, i=1 to 25
  corvector<-c(rep(0,25))
for (i in 1:25)
 {addD=ls_fit_addtree(eurodist,method=c("IP"), weights = 1, control = list())
modvec=as.vector(addD)
corvector[i]=cor(datvec,modvec)  # calculate linear fit = corr of data & model distances
}
corvector
# the vector of fits shows that local minima are very common (cf. Smith, 1998)

# =================================================
# You may want to save your R session history for future reference:
"c:/Users/corter/R_hist" <- tempfile("Rrawhist")
savehistory("c:/Users/corter/R_hist")

# refs:
#Smith, T. J. (1998). A comparison of three additive tree algorithms that rely on a least-squares loss criterion. 
#British Journal of Mathematical and Statistical Psychology, 51: 269?288. doi:10.1111/j.2044-8317.1998.tb00681.x
