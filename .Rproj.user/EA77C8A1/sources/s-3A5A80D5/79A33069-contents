# Eigenvalues and eigenvectors

A <- matrix(c(3, 4, -1, -1, -2, 1, 3, 9, 0), 3, 3, byrow = T)
E <- eigen(A)
E

# S3 class object
class(E)

# with an added attribute class as "eigen"
attributes(E)

# Simple list
unclass(E)


# eigenvector corresponding to minimum eigenvalue
E$vectors[, which.min(E$values)]
