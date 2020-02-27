# Inverse transform method


inv.transform <- function(n, inv.func, plot=T, pdf=NULL, func.str=NULL){
  rsamp <- inv.func(runif(n))
  if (plot){
    hist(rsamp, probability = T, 
         main=ifelse(is.null(func.str),
                     "",
                     paste("Inverse transform sampling for f(x) = ", func.str))
    )
    if (!is.null(pdf)){
      curve(pdf, add=T, col="red")
      
    }
  }
  return(rsamp)
}
#  Generate a random sample from from f(x) = 3x^2 0<x<1
pdf <- function(x){3*x^2}
inv.func <- function (x){
  return(x^(1/3))
}

inv.transform(1e5, inv.func, plot = T, pdf = pdf, func.str = "3x^2")

# Generate a random sample from exp(x) with parameter lambda
lambda.val <- 5
pdf <- function (x, lambda=lambda.val){
  return(dexp(x, rate = lambda))
}
inv.func <- function (x, lambda=lambda.val){
 return(-log(1-x)/lambda)
}

inv.transform(1e5, inv.func, plot=T, pdf=pdf, func.str="5exp(-5x)")

# F(x) = x^2 + x
pdf <- function(x){
  return( x + 1/2)
}

inv.func <- function(x){
  return((-1 + (1 + 8*x)^(1/2))/2)
}

inv.transform(1e5, pdf = pdf, 
              inv.func = inv.func, 
              func.str  = "x + 1/2")

# q4
pdf <- function(x){
  return(ifelse(x >= 2 & x <= 3, (x-2)/3, 
         ifelse(x >=3 & x<= 6,  (2- x/3)/2, 0),
         0))
}
inv.func <- function(x){
  return(ifelse(x >= 2 & x <= 3, (x-2)/3, 
                ifelse(x >=3 & x<= 6,  (2- x/3)/2, 0),
                0))                                             
}
