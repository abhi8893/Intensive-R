# Create a matrix with diag = NA, and lower = 10, upper = 20
A <- matrix(0, 10, 10)

set.tri.vals <- function(idx, upper.val, lower.val, diag.val){
  i <- idx[0]
  j <- idx[1]
  if (i > j){
    val <- upper.val
  } else if (i < j){
    val <- lower.val
  } else{
    val <- diag.val
  }
}

# Method 2
M <-  matrix(0, 10, 10)
diag(M) <- NA
M[lower.tri(M, diag = F)] <- 10
M[upper.tri(M, diag = F)] <- 20
