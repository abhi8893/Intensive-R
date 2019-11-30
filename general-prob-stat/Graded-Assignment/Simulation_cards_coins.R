#' ---
#' title: Assignment (Probability theory)
#' author: Abhishek Bhatia
#' date: "29th October 2019"
#' ---
#' 
#' 1. Find out the probability of head after tossing a fair coin.

# A general function to perform an experiment 
#  - consisting of nrep trials
#  - In each trial num.coins coins are tossed


coin.toss <- function(num.coins, nrep, event.idfunc){
  
  # Create an empty vector to
  # store the estimate of the probability in each trial
  probs <- vector("numeric", nrep)
  # H <- 1, T <- 0
  choices <- c(1, 0)
  for (i in 1:nrep){
    res <- sample(choices, num.coins, replace = T)
    # TODO: Define a general function to decide what to do with
    #       the res object? Maybe an S3 class.
    probs[i] <- event.idfunc(res)
  }
  p <- mean(probs)
  return(p)
}

has.consec <- function(res, val){
  last.val <- res[1]
  for (i in 2:length(res)){
    cur.val <- res[i]
    if (last.val == val & cur.val == val){
      return(TRUE)
    }
    last.val <- cur.val
  }
  return(FALSE)
}

has.consec(c(1, 0, 1, 1), 1)
max.nrep <- 2e3
res <- sapply(1:max.nrep, coin.toss, num.coins=4, 
              event.idfunc=function(res) has.consec(res, val=1))
plot(1:max.nrep, res, type='l')
abline(h=1/2, col="red")

card.draw <- function(num.cards, nrep, event.idfunc){
  # Create an empty vector to
  # store the estimate of the probability in each trial
  probs <- vector("numeric", nrep)
  # Card deck 
  choices <- lapply(c("H", "D", "S", "C"), 
                    function (nm) paste0(nm, 1:13))
  choices <- Reduce(c, choices)
  
  for (i in 1:nrep){
    res <- sample(choices, num.cards, replace = F)
    # TODO: Define a general function to decide what to do with
    #       the res object? Maybe an S3 class.
    probs[i] <- event.idfunc(res)
  }
  p <- mean(probs)
  return(p)
}

max.nrep <- 2e3

# First card is ace
res <- sapply(1:max.nrep, card.draw, num.cards=4, 
              event.idfunc=function(res) ifelse(substr(res, 2, 3) == '1',
                                                T, F))
plot(1:max.nrep, res, type='l')
abline(h=4/52, col="red")

