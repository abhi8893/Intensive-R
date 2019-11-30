# Where is NA?

replace.NA1 <- function(v, replace.with = 0){
  
  n <- 0
  len <- length(v)
  idx <- c()
  for (i in 1:length(v)){
    if (is.na(v[i])){
      v[i] <- replace.with
      n = n + 1
      idx <- c(idx, i)
    }
  }
  
  l <- list(vec=v, count=n, idx=idx)
  return(l)
}


replace.NA2 <- function(v, replace.with = 0){
  
  n <- 0
  len <- length(v)
  idx <- vector(mode="numeric", length=len)
  for (i in 1:length(v)){
    if (is.na(v[i])){
      v[i] <- replace.with
      n = n + 1
      idx[n] <- i
    }
  }
  
  idx <- idx[1:n]
  
  l <- list(vec=v, count=n, idx=idx)
  return(l)
}

replace.NA1(c(1, 2, NA, 3))
v <- sample(c(1, NA), 1e4, replace = T)

# allocating a space before hand is much faster
# Hence replace.NA2 is faster
microbenchmark::microbenchmark(
  replace.NA1(v),
  replace.NA2(v)
)
