# Curve function

f <- function(x){
  x^2
}

f.x <- curve(f, xlim=c(-2, 2), n = 1e3)
f.x
