# Ref: Pg 204 Art of R Programming
# E[max(X, Y)],  X, Y ~ N(0, 1)

find.exp1 <- function(nreps){
  return(mean(pmax(rnorm(nreps), rnorm(nreps))))
}

find.exp2 <- function(nreps){
  sum <- 0
  for (i in 1:nreps){
    xy <- rnorm(2)
    sum <- sum + max(xy)
  }
  
  return(sum/nreps)
}


microbenchmark::microbenchmark(
  find.exp1(1e5),
  find.exp2(1e5)
)
