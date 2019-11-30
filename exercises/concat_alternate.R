# Concatenate two vectors alternately
concat.alternate <- function(v1, v2){
  c(rbind(v1, v2))
}

v1 <- 1:10
v2 <- 11:20

concat.alternate(v1, v2)
