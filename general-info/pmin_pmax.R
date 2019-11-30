# pairwise minima and maxima
# NOTE: As many vectors can be compared not just 2.
z <- cbind(c(1, 5, 6), c(2, 3, 2))
min(z[, 1], z[, 2])
pmin(z[, 1], z[, 2])


z <- matrix(sample(1:10, 50, replace = T), nrow=5, ncol=10)

# Split a matrix into a list of column vectors
clmn.vecs <- split(z, rep(1:ncol(z), each = nrow(z)))

# Find row-wise minima
do.call(pmin, clmn.vecs)

# or using apply
apply(z, 1, min)


