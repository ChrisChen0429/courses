

# Class Script
# September 8, 2017


# Assign the value "x" to be 24^2

x <- 24^2
x
print(x)

x <- 1:50
x

# object_name <- value

X <- "HELLO"
X
print(c(x, X))





x <- 2
class(x)
class(2L)
class(2)

y <- as.integer(3)
class(y)

class("Columbia University")
class(FALSE)


vec <- c(0.5, 0.6)
vec
class(vec)

vec <- c(TRUE, FALSE)
class(vec)

vec <- c("Columbia", "University")
vec

vec <- 3:12
vec
vec <- rep(4, 5)


vec <- c(1.7, "a")
vec

vec <- c(TRUE, 2)
vec
class(vec)

vec <- c("a", TRUE)
vec
class(vec)


vec <- 0:4
class(vec)
as.numeric(vec)
class(vec)
as.character(vec)
class(vec)
as.logical(0:4)


vec <- c("a", "b", "c")
as.numeric(vec)



mat <- matrix(ncol = 3, nrow = 2)
mat
dim(mat)
attributes(mat)



mat <- matrix(1:6, ncol = 3, nrow = 2)
mat
mat <- matrix(1:6, ncol = 3, nrow = 2, byrow = TRUE)
mat

mat <- 1:10
mat
dim(mat) <- c(2,5) 
mat


x <- rep(5, 3)
y <- 11:13
cbind(x, y)
rbind(x, y)

lst <- list(1, "a", TRUE, 1+4i)
lst

vec <- c("Welcome", "to", "Columbia")
vec
names(vec)
names(vec) <- c("Word1", "Word2", "Word3")
vec

myList <- list(stuff = 3, mat = matrix(1:4, nrow = 2), moreStuff = c("china", "japan"), new_list = list(5, "bear"))
names(myList)


mat
dimnames(mat) <- list(c("a", "b"), c("c", "d", "e", "f", "g"))
# colnames()
# rownames()


x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
names(x) <- c("Cindy", "Rush")
x
row.names(x) <- c("00", "01", "10", "11")
x
