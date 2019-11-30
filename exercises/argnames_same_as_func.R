# Using argument names same as function names doesn't affect R like Python

my.rnorm <- function(n, mean=0, sd=1){
  return(rnorm(n, mean, sd))
}

my.rnorm(10, 1, 3)
