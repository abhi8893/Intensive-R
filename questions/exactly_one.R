# exactly one 
# Ref: Pg 190 Art of R Programming

# p is vec of probabilities of n events
# find the probability of exactly one occuring

# TODO: Create a function with exactly n occuring
exactly.one <- function(p){
  notp <- 1 - p
  tot <- 0
  for (i in seq_along(p)){
    tot <- tot + p[i]*prod(notp[-i])
  }
  return(tot)
}

# TODO: Create a generic function with constraint
# NOTE: Here the constraint is the sum of probs should be 1
#       and each prob should be positive.
# TODO: Implement sub array sums using cumsum
create.probs <- function(size){
  probs <- vector(mode = "numeric", length = size)
  probs[1] <- runif(1, 0, 1)
  s <- probs[1]
  for (i in 2:(size-1)){
    probs[i] <- runif(1, 0, 1 - s)
    s <- s + probs[i]
  }
  
  probs[size] <- 1 - s
  return(probs)
}


p <- create.probs(10)
p

exactly.one(p)
