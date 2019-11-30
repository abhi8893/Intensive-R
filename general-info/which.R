# which function
# returns the indices where TRUE
# NOTE: It excludes NA, as it only selects
# indices which are TRUE
# and NA on condition evaluates to NA

v <-  c(1, 2, 3, 6, NA, 5, 4, 0)
which(v >=3)

A <- matrix(v, 4, 2)

# vector form indices
which(A >= 3)

# Array like row-col indices
which(A >= 3, arr.ind = T)


