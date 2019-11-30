# Swap 2 values in a vector
source("mapvalues.R")

swap.values1 <- function(v, x1, x2){
  my.mapvalues(v, c(x1, x2), c(x2, x1))
}

v <- c("m", "s", "p", "s", "p")
swap.values1(v, "s", "p")


