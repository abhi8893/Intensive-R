# Simple outlier detection in a matrix
# find rowwise or colwise
# return the index of 

findols <- function(X, MARGIN){
  findol <- function(v){
    mdn <- median(v)
    devs <- abs(v - mdn)
    return(which.max(devs))
  }
  return(apply(X, MARGIN, findol))
}


A <- matrix(rnorm(9*10), nrow=9, ncol=10)
# rowwise
findols(A, 1)

# colwise
findols(A, 2)
