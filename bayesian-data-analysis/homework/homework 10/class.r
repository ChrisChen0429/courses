n <- 10000
x <- rnorm(n,0,5)
z <- rep(c(0,1),n/2,replacemen=TRUE)
a <- 1
b <- 0.7
theta <- (20 - 0.2 * x) + runif(n,0,5)
sigma <- 5
y <- a + b * x + theta * z + rnorm(n,0,sigma)

corrs <- rep(NA,2)
for (j in 1:2){
    corrs[j] <- cor(x[z==(j-1)],y[z==(j-1)])
}

par(tck=-0.01,mgp=c(1.5,0.5,0))
plot(x,y,bty='l',type='n')
for (j in 1:2){
    points(x[z==(j-1)],y[z==(j-1)],pch=20,col=j,cex=0.1)
}



model = function(x, z, a, b){
    y = rep(0, length(x))
    for (i in 1:length(x)){
        if (z[i] == 1){
            y[i] = a * x[i] + b
        } else {
            y[i] = x[i]
        }
    }
}