# Apply a function colwise or rowwise to a matrix of 1s and 0s, 
# and return a vector with corresponding element 1 or 0, depending 
# on if the majority (atleast p proportion) of element in the row 
# or col was 1 or 0.

A <- matrix(sample(c(0, 1), 4*4, replace =T),
            nrow=4, ncol=4)

# apply function
# Ref: Pg70 Art of R programming

convmaj <- function(v, p=0.5, na.rm=T){
  maj <- mean(v, na.rm=na.rm)
  return(ifelse(maj > p, 1, 0))
}

# rowwise 
apply(A, 1, convmaj, p=0.6)

# colwise
apply(A, 2, convmaj, p=0.6)
