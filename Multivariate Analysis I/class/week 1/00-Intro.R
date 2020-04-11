# Introduction to Multivariate Statistics
# HUDM 6122

# R Intro



# INSTALLING AND USING PACKAGES 
# Packages are collections of R functions, data, and compiled code bundled 
# up into a well-defined format that makes them easily importable into R.

library() # shows you which packages are available.

### Search the internet for details about package "car". Then install it.
install.packages('car') 
### If you haven't done so already, R will ask you which CRAN mirror
### to download from. After that it will be your default.
### The package car (companion to applied regression) will be installed, 
### along with any dependent packages it requires. At this point, although 
### we just *installed* the package, we have not yet *loaded* it into the 
### current workspace.

library(car)

### Once the packcage has been loaded you can use its functions and access
### its help and data examples (if any)

data() # Shows all example data sets currently loaded.

# Built-in dataset iris is a classical example of multivariate data
data(iris)
iris

# Data matrix:
X = iris[ ,1:4]
# Notice we selected only columns 1 to 4, as the fifth is non-numerical
X
dim(X) # X is a 150x4 matrix. That is, n = 150, p = 4

# The 4 column means
x.bar = colMeans(X)
x.bar

# To obtain the variances we can use the apply command.
# It applies any function on either the rows or columns of a matrix:
s.sq = apply(X, MARGIN = 2, var)
s.sq
# Notice MARGIN = 2 means to apply the function to the columns; use 1 for rows

# Covariance matrix:
cov(X)
# Note that the diagonal elements are exactly the variances
# Alternative method to obtain the variances:
diag(cov(X))

# Correlation matrix
cor(X)

# To make dataset names directly available we attach the data:
attach(iris)


# Graphs:
# 1. Histograms - plot one variable at a time
hist(Sepal.Width)

# Can be smoothed to a density estimate:
plot(density(Sepal.Width))

# 2. Scatterplots - two variables
plot(Sepal.Length, Sepal.Width)

# Can be done as a scatterplot matrix each pair of numerical variables:
pairs(iris[, 1:4])

# Or specific species
pairs(iris[101:150, 1:4])

# Another example with Motor Trend cars data:
pairs(~mpg+disp+drat+wt,data=mtcars, main="Simple Scatterplot Matrix")





# Better scatterplot matrix in the lattice library function splom
install.packages("lattice")
library(lattice)

splom(iris[101:150, 1:4])

# All species in different colors:
splom(iris[,1:4], groups = iris$Species)

# MT cars by number of cylinders
splom(mtcars[c(1,3,5,6)], groups=mtcars$cyl,
      panel=panel.superpose, col = 1:3,
      key=list(title="Three Cylinder Options",
               columns=3,
               points=list(pch=1,
                           col=1:3),
               text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))





# 3. Fancy combinations and beautifications
install.packages("ggpubr")
library(ggpubr)

# Scatterplot with marginal density plot
ggscatterhist(iris, x = "Sepal.Length", y = "Sepal.Width",
              color = "blue",
              margin.params = list(fill = "lightgray"))


# By species:
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 2, alpha = 0.5,
  palette = c("blue", "orange", "red"),
  margin.params = list(fill = "Species", color = "black", size = 0.3)
)



# Scatterplot matrix + densities + correlations
install.packages("GGally") 
GGally::ggpairs(mtcars[c(1,3,5,6)])








# 4. 3-D scatterplots

library(lattice)
attach(iris)
# Basic 3 color plot with arrows along axes
cloud(Petal.Length ~ Sepal.Length * Sepal.Width, data=iris, groups=Species)

# Split screen with all combinations
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, groups = Species, main = "1", pch=1:3,
            scales = list(draw = FALSE), zlab = "SL",
            screen = list(z = 30, x = -75, y = 0)),
      split = c(1, 1, 2, 2), more = TRUE)

print(cloud(Sepal.Width ~ Petal.Length * Petal.Width,
            data = iris, groups = Species, main = "2", pch=1:3,
            scales = list(draw = FALSE), zlab = "SW",
            screen = list(z = 30, x = -75, y = 0)),
      split = c(2, 1, 2, 2), more = TRUE)

print(cloud(Petal.Length ~ Sepal.Length * Sepal.Width,
            data = iris, groups = Species, main = "3", pch=1:3,
            scales = list(draw = FALSE), zlab = "PL",
            screen = list(z = 30, x = -55, y = 0)),
      split = c(1, 2, 2, 2), more = TRUE)

print(cloud(Petal.Width ~ Sepal.Length * Sepal.Width,
            data = iris, groups = Species, main = "4", pch=1:3,
            scales = list(draw = FALSE), zlab = "PW",
            screen = list(z = 30, x = -55, y = 0)),
      split = c(2, 2, 2, 2))




# Another example with MT cars
attach(mtcars)

# create factors with value labels
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4cyl","6cyl","8cyl")) 

# Split plot automatically
plot(cloud(mpg~wt*qsec|cyl.f,
           main="3D Scatterplot by Cylinders") )
detach(mtcars)








# 5. Graphs of growth curves for time series data or panel data

# Open file greenhousegas_pop_10nations.csv
gg <- read.csv(file.choose())
attach(gg)
gg_percap <- grnGasEm100K/pop10M
interaction.plot(year, country, gg_percap, col=as.numeric(code))
detach(gg)




# Female bear data from Table 1.4 on p. 24
# bears.dat
bears = read.table(file.choose())
# Rename the columns:
colnames(bears) = c("Wt2", "Wt3", "wt4", "Wt5", "Lngth2", "Lngth3", "Lngth4", "Lngth5")
bears
# Obtaining Figure 1.13
ts.plot(t(bears[1:7,1:4]), lty = 1:7, xlab = "Year", ylab = "Weight", main = "Combined growth curves for weight of 7 female grizzly bears")
legend(1, 240, legend=c("Bear 1", "Bear 2", "Bear 3", "Bear 4", "Bear 5", "Bear 6", "Bear 7"), lty=1:7, cex=0.8)








# 6. Star plots
# Portray the value of each normalized variable as a point along the a ray from the center

data(mtcars)
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2), main = "Motor Trend Cars", full = TRUE) 

# In color
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2),
      main = "Motor Trend Cars", draw.segments = TRUE, full = TRUE)

# First change the palette which is used when a col= has a numeric index.
palette(rainbow(12, s = 0.6, v = 0.75))

# Segment Diagrams:
stars(mtcars[, 1:7], len = 0.6, key.loc = c(1.5, 0), main = "Motor Trend Cars", draw.segments = TRUE,
      nrow = 4, cex = .7)

# Controlling various parameters:
# len	is a scale factor for the length of radii or segments.
# draw.segments = TRUE produces a segment diagram instead of a star/spider plot

# Another example with USJudgeRatings dataset
## scale linearly (not affinely) to [0, 1]

# User scaling to [0, 1]
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
Jnam <- row.names(USJudgeRatings)
# Abreviate names
Snam <- abbreviate(substring(Jnam, 1, regexpr("[,.]",Jnam) - 1), 7)
stars(USJudge, labels = Snam, scale = FALSE,
      key.loc = c(13, 1.5), main = "Judge", len = 0.8, cex = 0.5)
palette("default")

# 7. Chernoff faces
# Depict each variable as a feature on a face.

install.packages("aplpack",dependencies = T)
library(aplpack)

# Built-in dataset Motor Trend cars
faces(mtcars)

# The face of crime
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)
faces(crime[,2:8])

# Add lables:
faces(crime[,2:8], labels = crime$state, cex = 0.8)


# Other visualization methods NOT covered in class

# 8. 2-D histograms

install.packages("hexbin")
library(hexbin)

# Bivariate normal
x <- matrix(rnorm(4000), 2000, 2)
plot(hexbin(x[,1], x[,2]))

# Some other joint distribution
x <- rnorm(10000)
y <- rnorm(10000)

plot(hexbin(x, y + x*(x+1)/4), main = "(X, X(X+1)/4 + Y)  where X,Y ~ rnorm(10000)")

# 9. Andrews curves

# Andrews plot defines a Fourier series transformation of the data
# If there is structure or difference between subsets it will become visible
# Also used to identify outliers (similar to X-outliers in regression)

install.packages("andrews")
library(andrews)
data(iris)

# Iris dataset by species type (column 5)
andrews(iris,clr=5,ymax=3)

# There are different defintions of the formula
andrews(iris,type = 4, clr = 5, ymax = 2)

# 10. Parallel coordinates 
# Rather than orthogonal axes parallel coordinates represent axes as equidistant parallel lines

library(MASS)
parcoord(iris[,1:4][, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)

