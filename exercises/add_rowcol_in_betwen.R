# Add a column in between in a matrix

add.rowcol <- function(M, v, index, row=FALSE){
  if (row){
    M <- rbind(M, v)
    M[c(index, nrow(M)), ] <- M[c(nrow(M), index), ]
  } else {
    M <- cbind(M, v)
    M[, c(index, ncol(M))] <- M[, c(ncol(M), index)]
  }
  
  return(M)

}
A <- matrix(c(1, 2, 3), 3, 3, bycol=TRUE)
v <- c(0, 0, 0)

add.rowcol(A, v, 2, row=FALSE)
