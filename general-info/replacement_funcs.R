# Replacements functions
# Ref: Pg 182 Art of R Programming

x <- c(1, 2, 4)
names(x)

# How on earth can you assign to a return value of a function?
names(x) <- c("a", "b", "c")
x

# There is actually a function named "names<-"
x <- "names<-"(x, value=c("d", "e", "f"))
x

# Even this is a replacement function example
x[2:3] <- c(0, 9)
x

x <- "[<-"(x, 2:3, c(-5, -4))
x

# A self-bookkeeping vector class 
# Ref: Pg 184 Art of R Programming

newbookvec <- function(x){
  tmp <- list()
  tmp$vec <- x
  tmp$wrts <- vector(mode="numeric", length=length(x))
  class(tmp) <- "bookvec"
  return(tmp)
  
}


# read function
"[.bookvec" <- function(bv, idx){
  return(bv$vec[idx])
}

# write function
"[<-.bookvec" <- function(bv, idx, vals){
  bv$wrts[idx] <- bv$wrts[idx] + 1
  bv$vec[idx] <- vals
  return(bv)
}

b <- newbookvec(1:10)
b
class(b)

b[2]

# BUG: NOT WORKING
b[2] <- -2

b <- "[<-.bookvec"(b, 2, -2)
