# Kendall's tau
# Proportion of time both increase or decrease together
# Ref: Pg49 Art of R programming


findud <- function(v){
  # Successive differences
  # diff(v) <- slower?
  vud <- v[-1] - v[-length(v)]
  # Convert inc to 1 dec to -1
  # sign(vud)
  return(ifelse(vud > 0, 1, -1))
}

udcorr <- function(v1, v2){
  ud <- lapply(list(v1, v2), findud)
  return(mean(ud[[1]] == ud[[2]], na.rm = T))
}

v1 <- c(5, 12, 13, 3, 6, 0, 1, 15, 16, 8, 88)
v2 <- c(4, 2, 3, 23, 6, 10, 11, 12, 6, 3, 2)

udcorr(v1, v2)
