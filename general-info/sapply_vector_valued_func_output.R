# vector vaiued function output and sapply

square <- function(x) return(c(x, x**2))

m <- sapply(1:8, square)
m.t <- t(m)
m.t
