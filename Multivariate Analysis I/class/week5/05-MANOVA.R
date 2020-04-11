# Ex. 6.7 on p. 298)
g1 = c(9, 6, 9)
g2 = c(0, 2)
g3 = c(3, 1, 2)
Data <- data.frame(Y = c(g1, g2, g3), 
                   Group = factor(rep(c("g1", "g2", "g3"), times=c(length(g1), length(g2), length(g3)))))
Data
m1 = aov(Y~ Group, data=Data)
anova(m1)

# Read QueenslandFuel.txt data
x <- read.table(file.choose(), header = TRUE)

# ANOVA
summary(aov(Price ~ City, data = x))
# Do not reject Ho

# Manual computation

m1 <- mean(x[1:9, 3])
m2 <- mean(x[10:18, 3])
m3 <- mean(x[19:27, 3])
m4 <- mean(x[28:36, 3])
m5 <- mean(x[37:45, 3])
m6 <- mean(x[45:54, 3])
(x.bar = mean(x$Price))

n1 = n2 = n3 = n4 = n5 = n6 = 9
g = 6

SS.mean = (n1 + n2 + n3 + n4 + n5 + n6)*((x.bar)^2)
(SS.Total = sum(x$Price^2) - SS.mean)

SS.Treat = n1*((m1 - x.bar)^2) + n2*(m2 - x.bar)^2 + n3*(m3 - x.bar)^2 + n4*(m4 - x.bar)^2 + n5*(m5 - x.bar)^2 + n6*(m6 - x.bar)^2
SS.Treat
(MS.Treat = SS.Treat/(g - 1))

(SS.Res = SS.Total - SS.Treat)
(MS.Res = SS.Res/(n1 + n2 + n3 + n4 + n5 + n6 - g))
(F.Test = MS.Treat/MS.Res)
