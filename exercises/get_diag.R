# Diagonal elements in a matrix

A <-  matrix(1:25, 5, 5)

# Get diagonal elements
A[row(A) == col(A)]

# Replace diagonal with NA
A[row(A) == col(A)] <- NA

# Get array indices
which(row(A) == col(A), arr.ind = T)
