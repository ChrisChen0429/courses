# Creating vectors and matrices

vec1 <- c(1,2,3,4,5,6)
vec1

# Equivalently:
vec1 = 1:6

# The matrix command may be used to create a matrix in R. The first argument is a vector
# that you want converted into a matrix. The second and third arguments are 
# the number of rows    and columns the matrix will have. The fourth argument is 
# whether you want the vector to fill the matrix by moving across rows or down columns.

A <- matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
A

# Accessing elements of the matrix:

A[2,3]
A[2,]  # Second row
A[,3]  # Third column

# The transpose is taken by using the t function.
t(A)

# 1.	Matrix Addition, Subtraction, and Multiplication

B <- matrix(c(6, 7, 8, 9, 10, 11), nrow = 2, ncol = 3, byrow = TRUE) 
B
A + B
5*B

C <- matrix(10:21, nrow = 3, ncol = 4, byrow = TRUE) 
C

# Matrix multiplication in R is run by a combination of three characters %*%. 
# This is to distinguish it from arithmetic (or scalar) multiplication. 
# Define a 3?4 matrix C below. Note that it is conformable with A because 
# A has 3 columns and C has 3 rows.

A %*% C

# 2. Rank and linear dependence

# It can be obtained as a byprosuct of the QR decomposition function:
qr(A)$rank

# Example from class:
x1 = c(1, 1, 1)
x2 = c(2, 5, -1)
x3 = c(0, 1, -1)

2*x1 - x2 + 3*x3


# 3. Determinants

A <- matrix( c(1,2,3,2,2,1,3,1,4), nrow = 3, ncol = 3, byrow = TRUE)
A
det(A) 

# 4. Diagonal and identity matrices
I2 <- diag(2)
I2
I3 <- diag(3) 

# Note multiplication with identity does not change the matrix
I3 %*% A

D6 <- diag(c(1,2,3,9,8,7)) 
D6

# 5. Matrix inverse
# The solve function is used to take the inverse in R.
Ainv <- solve(A)
Ainv

# Check if AAinv is identity:

round(A%*%Ainv, 2)
# Note the computer precision and as a result zeros being very small numbers!

# Example from class about solving system of linear equations:

A = matrix(c(12, 10, 6, -2), 2, 2)
C = c(48, 12)
solve(A, C)

# Trace

sum(diag(A))

# Eigenvalues and Eigenvectors
A <- matrix(c(3,2,1,2,3,2,1,2,3), nrow = 3, ncol = 3, byrow = TRUE) 
A
evd <- eigen(A)
evd

# When data are stored in an R object, such as evd,  defined above as the 
# eigen decomposition of A, subsets of the data are stored in different 
# named attributes. The dollar sign $ in the output above tells us that 
# the eigenvalues are stored in evd$values and the eigenvectors are stored in evd$vectors

evd$values
evd$vectors

# Verify:
lambda1 <- evd$values[1] 
v1 <- evd$vectors[,1] 
A %*% v1
lambda1 * v1

# Spectral decomposition

# The spectral decomposition states that any symmetric matrix M may be decomposed 
# into the product of an orthogonal matrix C, a diagonal matrix D, and the transpose of C

M <- matrix(c(1,2,3,2,5,7,3,7,1), nrow = 3, ncol = 3, byrow = TRUE) 
M
evM <- eigen(M) 
C <-	evM$vectors
D <- diag(evM$values) 
D
# Check:
C %*% D %*% t(C)

# Square root of a matrix
# Note we need a positive definite matrix!

A = matrix(c(2, -1, 0, -1, 2, -1, 0, -1, 2), 3, 3)
A
# The easiest way to check is to see if all eigenvalues are positive:
eigen(A)

D.root = diag(sqrt(eigen(A)$values))
A.root = eigen(A)$vectors %*% D.root %*% t(eigen(A)$vectors)
# Check:
round(A.root %*% A.root, 2) == A
