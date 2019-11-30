# Implement Set operations

A <- 1:10
B <- 5:15

union(A, B)
intersect(A, B)
setdiff(A, B)

# Show that (A\B) U (B\A) U (A intersect B) = A U B

lhs <- Reduce(union, 
              list(setdiff(A, B), 
                   setdiff(B, A), 
                   intersect(A, B)))
rhs <- union(A, B)
setequal(lhs, rhs)

# setequal
A <- c(1, 2, 3, 4)
B <- c(3, 4, 1, 2)

check.setequal <- function(A, B){
  if (length(A) != length(B)){
    return(FALSE)
  }
  
  return(identical(sort(A), sort(B)))
}

check.setequal(A, B)
