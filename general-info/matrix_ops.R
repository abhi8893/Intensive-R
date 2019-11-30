# Some matrix ops
A <- matrix(c(11, 12, 21, 22, 31, 32), 3, 2, byrow=T)
A

# This will assign by col (since it's default)
A[c(1, 3), ] <- c(1, 2, 3, 4)
A

# This will assign by row, since we passed a matrix of same dim
A[c(1, 3), ] <- matrix(c(1, 2, 3, 4), 2, 2, byrow=T)


# NA matrix
x <- matrix(nrow=3, ncol=3)
y <- matrix(c(4, 5, 2, 3), nrow=2)

x[2:3, 2:3] <- y
x

# row and col number
row(A)
col(A)

z <- matrix(1:8, 4, 2)

# extracts as vector
r1 <- z[2,]
attributes(r1)

# extracts as matrix
r2 <- z[2,, drop=FALSE]
attributes(r2)

# even "[" is a function
# BUG: Rstudio will show an error 
"["(z, 2, , drop = FALSE)

# another way to extract as a matrix
r1 <- t(as.matrix(r1))
