## Example 8.3 on p. 443 (Madison, Wisconsin data):
# Read data from Table 8.5:
d = as.matrix(read.table("T8-5.DAT"))
colnames(d) = c("Pop.Thousands", "ProfDegree.Percent", "Employed16.Percent", 
                "GovEmployed.Percent", "MHV.HundredThousands")
d

# Covariance matrix:
(S =cov(d))

#PCA:
(pca = prcomp(d))

# Variance of each component:
round(eigen(S)$values, 2)

# Cumulative percent explained total variance:
for (i in 1: ncol(d)) print(sum(eigen(S)$values[1:i])/sum(eigen(S)$values))
# We can use just 2 components!

# Interpretation: 
# PC1: Difference between government and overall employment
# PC2: Sum of two employment percents

# Screeplot:
#install.packages("factoextra")
library("factoextra")

fviz_eig(pca)
# Look where the graph "flattens" (elbow)


## Example 8.5 on p. 451 (stocks returns):
# Read data from Table 8.4:
stocks = as.matrix(read.table("T8-4.DAT"))
colnames(stocks) = c("JPM", "C", "WFC", "RDS", "XOM")
stocks

# Correlation matrix:
# (R = matrix(c(1, 0.632, 0.511, 0.115, 0.155, 
#              0.632, 1, 0.574, 0.322, 0.213,
#              0.511, 0.574, 1, 0.183, 0.146,
#              0.115, 0.322, 0.183, 1, 0.683,
#              0.155, 0.213, 0.146, 0.683, 1), 5, 5))
(R = cor(stocks))

eigen(R)
eigenvec = eigen(R)$vectors
rownames(eigenvec) = colnames(stocks)
round(eigenvec, 2)

# Let's say we want to use the first two components:
# y1 = -0.47*JPM - 0.53*C - 0.47*WFC -0.39*RDS - 0.36*XOM
# y2 =  0.37*JPM + 0.24*C + 0.32*WFC -0.59*RDS - 0.61*XOM

# Accounted variance:
sum(eigen(R)$values[1:2])/sum(eigen(R)$values[1:5])
# 76.9%

# Interpretation:
# PC1: Index or stock market
# PC2: Contrast between banking and oil sectors

## Confidence intervals for lambdas:
# Example 8.8 on p.457
# Note their lambda1 value is wrong!!

n = nrow(stocks)
(l1 = eigen(R)$values[1])
alpha = 0.05
(z.crit = qnorm(1-alpha/2))

# CI lower:
l1/(1 + z.crit*sqrt(2/n))

# CI upper:
l1/(1 - z.crit*sqrt(2/n))

## More graphs:
## Example 3: USArrests
data(USArrests)
head(USArrests, 10)
# number of arrests per 100,000 residents for Assault, Murder, and Rape in 1973. 
# The data set also contains the percentage of the population living in urban areas, UrbanPop.

apply(USArrests, 2, var)
# Good idea to rescale the data!

# PC:
library("factoextra")
(pca.arrests = prcomp(USArrests, scale. = T))
summary(pca.arrests)

fviz_eig(pca.arrests)
# We will keep two PCs

# Interpretation:
# PC1: Overall crime rate
# PC2: Level of urbanization

# Graph of individual observations
fviz_pca_ind(pca.arrests,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# These are the plotted coordinates using 2 PC:
rescaled = apply(USArrests, 2, scale)
PC1 <- as.matrix(rescaled) %*% eigen(cov(rescaled))$vectors[,1]
PC2 <- as.matrix(rescaled) %*% eigen(cov(rescaled))$vectors[,2]
PC = cbind(PC1, PC2)
rownames(PC) = rownames(USArrests)
head(PC)

# Graph of variables:
fviz_pca_var(pca.arrests,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# Notice the coordinates are the correlations of each component with each variable!

# Results for Variables
res <- get_pca_var(pca.arrests)
res$coord          # Coordinates aka correlations
res$contrib        # Contributions to the PCs aka squared eigenvector coefficients*100
res$cos2           # Quality of representation 

# Biplot of individuals and variables:
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


## Example 4:
# Data on breast cancer from http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
# Read the file "wdbc.data"

wdbc <- read.csv("wdbc.data", header = F)

variables = c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(variables,"_mean"), paste0(variables,"_se"), paste0(variables,"_worst"))
head(wdbc)

# Explanation of the variable names from the website:
# “The mean, standard error, and “worst” or largest (mean of the three largest values) 
# of these features were computed for each image, resulting in 30 features. 
# For instance, field 3 is Mean Radius, field 13 is Radius SE, field 23 is Worst Radius.”

# PC on standardized data:
wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
summary(wdbc.pr)

# Screeplot
screeplot(wdbc.pr, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
# Note that when using correlation matrix R it makes sense to keep PC with variance > 1
# Otherwise the PC with less than 1 lambda explains less variance than 1 original variable

# Explained variance using first 6 PCs:
cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

# Using just 2 PCs:
plot(wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

# Fancier version of the same plot:
fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             fill.ind = wdbc$diagnosis, # does everything by diagnosis
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 vars dataset") + # add a title
  theme(plot.title = element_text(hjust = 0.5)) # center the title

# With just the first two components we can clearly see some separation 
# between the benign and malignant tumors!


# Exercise (Q.8.22 on p. 476):
# Use the bulls data in Table 1.10. There are 7 variables of interest in columns 3-9:
# "YrHgt", "FtFrBody", "PrctFFB", "Frame", "BkFat", "SaleHt", "SaleWt"
# Column 1 is the breed index (coded 1 for Angus, 5 for Hereford, and 8 for Simental) 
# Column 2 is the sales price, which we ignore

# a) Read the data, rename the columns appropriately and convert Column 1 to a factor

# b) Obtain the covariance matrix S

# c) Obtain the 7 eigenvalues of S

# d) Decide how many PCs to keep, based on % explained variance and the screeplot

# e) Interpret the PCs you decided to keep

# f) Use the first two PCs to create a scatterplot. Distinguish the 3 breeds with ovals.

# g) Are there outliers in the data? Answer based on scatterplot and Q-Q plot.

# h) Repeat from part b), but this time with the correlation matrix R