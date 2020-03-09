# Inverse transform functions

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