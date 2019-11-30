# Ref: The Art of R Programming Pg 37
# Predict the next value as 1 or 0 based on the previous k values
# If the previous k values has atleast k*p values as 1 predict 1,
# else predict 0. p <- cutoff probability

# This means we can predict n-k values
# TODO: Do it when there are NA values
# TODO: Refactor by initializing an empty NA_integer_ vector of length n

preda <- function(v, k, p=0.5){
  n <- length(v)
  # numeric bc we will predict 0 if cond not TRUE
  # numeric vec initializes to 0
  pred <- vector(mode = "numeric", length = n)
  for (i in 1:(n-k)){
    if (sum(v[i:(i+k-1)]) >= k*p){ # Inefficient to calculate sum of
      pred[i+k] <- 1               # sub-arrays everytime
    }
  }
  pred[1:k] <- NA
  return(pred)
}


# A bit more efficient method to calculate 
# sum of sub-arrays
predb <- function(v, k, p){
  n <- length(v)
  pred <- vector(mode="numeric", length = n)
  thresh <- k*p
  sm <- sum(v[1:k])
  if (sm >= thresh){
    pred[1+k] <- 1
  }
  
  for (i in 2:(n-k)){
    sm <- sm - v[i-1] + v[i+k-1]
    if (sm >= thresh){
      pred[i+k] <- 1
    }
  }
  
  pred[1:k] <- NA
  return(pred)
}

predc <- function(v, k, p){
  n <- length(v)
  pred <- vector(mode = "numeric", length = n)
  thresh <- k*p
  # Because there exists no subarray before index 1
  csm <- c(0, cumsum(v))
  
  for (i in 1:(n-k)){
    sm <- csm[i+k] - csm[i]
    if (sm >= thresh){
      pred[i+k] <- 1
    }
    
  pred[1:k] <- NA
  return(pred)
  }
}

if (interactive()){
  v <- c(1, 1, 0, 0, 1, 1, 1)
  pred1 <- preda(v, k=2, p=0.5)
  pred2 <- predb(v, k=2, p=0.5)
  pred3 <- predc(v, k=2, p=0.5)
  
  # TODO: Check if all are identical
  identical(pred1, pred2)
  identical(pred1, pred3)

  # Calculate MAE
  # TODO: Other accuracy metrics
  mean(abs(pred1 - v), na.rm = T)
  
  # time: preda > predb > predc
  v <- sample(c(0, 1), size = 1e4, replace = T, prob = c(0.3, 0.7))
  microbenchmark::microbenchmark(
    preda(v, k = 10, p = 0.5),
    predb(v, k = 10, p = 0.5),
    predc(v, k = 10, p = 0.5)
  )
  
  v1 <- c(1, 2)
  v2 <- 1:2
  
  # Be Careful with identical
  identical(v1, v2)
  all(v1 == v2)
  
  # This is because 
  typeof(v1) # double
  typeof(v2) # integer
}

