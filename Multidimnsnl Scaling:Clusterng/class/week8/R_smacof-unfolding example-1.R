
# using smacof for unfolding in R:

install.packages("smacof")
library(smacof)
mds(nat.dis,method="manhattan")

setwd("C:/Users/corter/Desktop/HUDM5124")
# unfolding examples
beverage <- read.table("ex_rect_data.txt",header=TRUE)
beverage
dim(beverage)
bev <- smacofRect(beverage[,2:6],conditionality="matrix")
bev
plot(bev, xlim = c(-3, 3), asp = 1)


# note - on this small example, the algorithm chokes when trying to run it as row-conditional
bev <- smacofRect(beverage[,2:6],conditionality="row")
bev
plot(bev, xlim = c(-3, 3), asp = 1)
