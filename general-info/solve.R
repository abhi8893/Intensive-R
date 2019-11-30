# Solve linear eq

# x - y = 2
# x + y = 4

# X = A_inv * B
a <- matrix(c(1, -1, 1, 1), nrow=2)
b <- c(2, 4)

X <- solve(a, b)
X # NOTE: It's a vector

X <- solve(a) %*% b
X # NOTE: it's a matrix (transposed vector)
