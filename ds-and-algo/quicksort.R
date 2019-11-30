# quicksort implementation
# Ref: Pg 176 Art of R Programming
# Recursion is memory intensive

qs <- function(x) {
  # NOTE: <= 1, o.w. qs will keep calling itself on empty vectors
  if (length(x) <= 1) return(x)
  
  pivot <- x[1]
  therest <- x[-1]
  sv1 <- therest[therest < pivot]
  sv2 <- therest[therest >= pivot]
  sv1 <- qs(sv1)
  sv2 <- qs(sv2)
  
  return(c(sv1, pivot, sv2))
}

qs(c(5, 4, 12, 13, 3, 8, 88))
