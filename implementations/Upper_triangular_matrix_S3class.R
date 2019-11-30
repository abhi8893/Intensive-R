# Implement a class for UT matrices
# Ref: Pg 214 Art of R Programming

sum1toi <- function(i) return(i*(i+1)/2)

# Constructor for creating a upper triangular matrix of class "ut"
ut <- function(inmat){
  n <- nrow(inmat)
  # Our return value is a list since it is a S3 class constructor
  rtrn <- list()
  class(rtrn) <- "ut"
  
  # Initializing a vector to store non zero elements
  num.nonzero <- sum1toi(n)
  rtrn$mat <- vector(length=num.nonzero)
  
  # Storing the index of where each column starts in rtrn$mat
  rtrn$ix <- sum1toi(0:(n-1)) + 1
  
  # Now start storing column i in rtrn$mat
  # The below for loop could be replaced by:
  #   inmat[row(inmat) <= col(inmat)]
  for (i in 1:n){
    num.coli <- i
    start <- rtrn$ix[i]
    end <- start + num.coli - 1
    # Since only 1:i indices of inmat in ith are non zero
    rtrn$mat[start:end] <- inmat[1:i, i]
  }
  return(rtrn)
}

# uncompress utmat to a full matrix
expandut <- function(utmat){
  n <- length(utmat$ix)
  fullmat <- matrix(nrow=n, ncol=n)
  # Start filling the matrix columns
  for (i in 1:n){
    start <- utmat$ix[i]
    end <- utmat$ix[i] + i - 1
    above.diag <- utmat$mat[start:end]
    fullmat[, i] <- c(above.diag, rep(0, n-i))
  }
  
  return(fullmat)
}

# print the utmat object of class "ut"
print.ut <- function(utmat){
  print(expandut(utmat))
}


# Multiplying two upper triangular matrices [objects of class "ut"]
"%mut%" <- function(utmat1, utmat2){
  
}