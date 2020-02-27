# Inverse transform (Discrete case)

sprt <- c(1, 2, 3, 4)
probs <- c(0.2, 0.15, 0.25, 0.4)
cum.probs <- cumsum(probs)
cdf <- approxfun(sprt, cumsum(probs), method="constant")


# TODO: Vectorize
inv.transform.discrete <- function (n, sprt, probs){
  rsamp <- vector("numeric", n)
  unif <- runif(n)
  cum.probs <- cumsum(probs)
  i <- 1
  for (u in unif){
    j <- 1
    for (p in cum.probs){
      if (u <= p){
        rsamp[i] <- sprt[j]
        break
      }
      j <- j +1
    }
    i <- i + 1
  }
  
  return(rsamp)
}

rsamp <- inv.transform.discrete(1e3, sprt, probs)
freq <- table(rsamp)/length(rsamp)
