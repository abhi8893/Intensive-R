# Closures

counter <- function(init=0){
  ctr <- init
  f <- function(do=c("show", "next")){
    do <- match.arg(do)
    if (do == "next"){
      ctr <<- ctr + 1
    }
    cat("the count currently has value", ctr, "\n")
  }
  
  return(f)
}

# Each time counter() if called, the variable ctr will be in a different
# environment. So c1 and c2 serve as independent counters.
# TODO: Use debugging to find the environments of f for c1 and c2
c1 <- counter()
c2 <- counter()
c1("show")
c1("next")
c1("show")

