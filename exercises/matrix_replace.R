# Fill NA or some val where condition is not T for a matrix

replace.val1 <- function(m, cond, val=NA){
  m[!cond] <- val
  m
}

