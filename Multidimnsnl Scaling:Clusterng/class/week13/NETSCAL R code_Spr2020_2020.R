
# R code for NETSCAL algorithm to determine structure of directed graphs
# by James E. Corter,  April 2017
# uses library igraph
# install.packages("igraph")
library(igraph)
help("igraph")

# read in matrix of dissimilarities - based on TC dept social nominations data
setwd("C:/Users/corter/Desktop/HUDM5124")
D<-read.csv(file="assn12dissims.txt",header=TRUE)
D

# for Assignment 13 (TC depts data), there are n=5 objects/nodes
n<-ncol(D)

# use the NETSCAL algorithm to define the "essential" arcs in G
# record the structure in the incidence matrix INC
INC<-matrix(rep(0,n^2),n,n)
INC
for (i in 1:n)  { 
for (j in 1:n)   {
if (i != j) {
check<-D[i,j]
linkij=TRUE
for (k in 1:n)
{ if ((k != i) & (k != j))
{ 
maxlink<-max(D[i,k],D[k,j])
if (maxlink<check) { linkij<-FALSE }
print(c(i,j,k,linkij,check,maxlink,D[i,k],D[k,j]))
}}
if (linkij==TRUE) { INC[i,j]=1 }
 } } } 
rownames(INC)=rownames(D)
colnames(INC)=colnames(D)
INC   # incidence matrix of graph with "necessary" nodes

# now we wish to create a graph from the incidence matrix INC
g_a12<-graph_from_adjacency_matrix(INC,mode="directed")
plot(g_a12)

# write plot of directed graph to a PDF file		
summary(g_a12)				
pdf("g_a12.pdf")				
plot(g_a12)
# dev.off()   # shut down the pdf device ??


# OR, we can use a list of edges to create the directed graph 
# first, count number of arcs in graph by summing entries in incidence matrix
 nlinks<-sum(INC)
# now, represent graph as a table (gg) of defined arcs
gg<-matrix(c(rep(0,nlinks*3)),nlinks,3)
nlinks=0
for (i in 1:n) 
{ for (j in 1:n) 
  { if (INC[i,j]==1) 
    { nlinks=nlinks+1
       gg[nlinks,1]<-i
       gg[nlinks,2]<-j
       gg[nlinks,3]<-1
  } } }
# gg is now a list of edges
g_as12<-graph_from_data_frame(gg, directed = TRUE, vertices = NULL)
plot(g_as12,vertex.label=rownames(INC))
gg
