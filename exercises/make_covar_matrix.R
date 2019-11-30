# Make a variance-covariance matrix

makecov <- function(rho, n){
  m <- matrix(nrow=n, ncol=n)
  m <- ifelse(row(m) == col(m), 1, rho)
  return(m)
}

makecov(0.5, 3)

