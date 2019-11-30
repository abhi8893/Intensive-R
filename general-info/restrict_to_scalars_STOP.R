# Restrict operations to scalars

add.c <- function(v, c){
  if (length(c) > 1) stop("c should be a scalar")
  return(v+c)
}
