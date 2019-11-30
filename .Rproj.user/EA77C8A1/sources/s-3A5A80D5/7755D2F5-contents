# Efficient matching of two vectors
# TODO: Find an inbuilt function written in C which does this

v1 <- 1:100000
v2 <- c(1:5, 0, 7:100000)

# TODO: Python like enumerate?


is.equal <- function(v1, v2){
  for (i in 1:length(v1)){
    if (v1[i] != v2[i]){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

is.equal(v1, v2)

# YAY! My function came 2nd!
microbenchmark::microbenchmark(is.equal(v1, v2), 
                               all(v1 == v2), 
                               identical(v1, v2),
                               all.equal(v1, v2))
