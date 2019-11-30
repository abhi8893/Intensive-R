# Consec runs of atleast k length
# runs should be contained within each other

consec.runs1 <- function(v, k){
  
  

  n <- length(v)
  runs <- NULL
  
  for (i in 1:(n-k+1)){
    if (all((v[i:(i+k-1)] == 1))){
      runs <- c(runs, i) # Inefficient
    }
  }
  
  return(runs)
}


# A more efficient way

consec.runs2 <- function(v, k){
  n <- length(v)
  # Efficient to initialize an empty vector
  # rather than append to an array
  runs <- vector(length=n)
  
  count <- 0
  for (i in 1:(n-k+1)){
    if (all(v[i:(i+k-1)] == 1)){
      count <- count + 1
      runs[count] <- i
    }
  }
  runs <- runs[1:count] # More efficient to discard at the end
  return(runs)
}

if (interactive()){
  consec.runs1(c(1, 0, 0, 1, 1, 1, 0, 1, 1), k=2)
  consec.runs2(c(1, 0, 0, 1, 1, 1, 0, 1, 1), k=2)
  
  v <- sample(c(0, 1), 3*10000, replace = T)
  
  microbenchmark::microbenchmark(
    consec.runs1(v, 2),
    consec.runs2(v, 2)
)
}
