# Exercise:
# Read datasaet from T10_1_CHEM.DAT
require(CCA)
d <- as.matrix(read.table("T10_1_CHEM.DAT"))
colnames(d) = c("Number", "Unchanged", "Converted", "Unwanted", "Temperature", "Concentration", "Time")

# X1 varaibles "Temperature", "Concentration", "Time" with interaction effects and quadratic terms
X1 <- cbind(d[, 5:7], d[,5]*d[,6], d[,5]*d[,7], d[,6]*d[,7], d[, 5:7]^2)
colnames(X1) = c("Temperature", "Concentration", "Time", "Temp.Conc", "Temp.Time", "Conc.Time", "Temp.Sq", "Conc.Sq", "Time.Sq")
X1

# X2 variables "Unchanged", "Converted", "Unwanted"
X2 <- d[, 2:4]
# a) Find all correlations between X1 and X2 

# b) Find the canonical correlations

# c) Find the coefficients of U and V variates for all canonical correlations
# USE cc function fro the package CCA!!
# d) EXTRA CREDIT:
# Report in the chat the highest absolute value coefficient in U1

# a) All correlations
cor(X1,X2)

# b)
cc(X1, X2)$cor
# c)
cc(X1, X2)$xcoef
cc(X1, X2)$ycoef
