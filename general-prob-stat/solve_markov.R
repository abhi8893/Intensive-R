stnry.distr <- function(P){
  n <- nrow(P)
  A <- rbind(P - diag(1, n), 1)
  b <- vector("numeric", n+1)
  b[n+1] <- 1
  x <- drop(solve(t(A) %*% A, t(A) %*% b))
  return(x)
}


stnry.distr(P)
