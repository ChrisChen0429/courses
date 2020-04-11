# Example 7.4 on p. 372
# Read data from Table 7.1

realestate <- read.table("T7-1.DAT", 
                   col.names = c('Size', 'Value', 'Price'))
realestate

n = nrow(realestate)
r = 2
(Z = as.matrix(cbind(rep(1, n), realestate[, 1:2])))
y = realestate[,3]

solve(t(Z) %*% Z)

(b.hat = solve(t(Z) %*% Z) %*% t(Z) %*% y)

SS.res = t(y - Z %*% b.hat) %*% (y - Z %*% b.hat)/(n-r-1)
sqrt( SS.res)

# Built-in function

reg = lm (Price ~ Size + Value, data = realestate)
summary(reg)
