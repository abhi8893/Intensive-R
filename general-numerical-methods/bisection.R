# Implementing the bisection method


endpt.are.roots <- function(f, a, b){
  if (f(a) == 0){
    return(a)
  } else if (f(b) == 0){
    return(b)
  }
  
  return(NULL)
}

get.next.interval <- function(f, a, b){
  mid.pt <- (a + b)/2
  
  if (f(a)*mid.pt < 0){
    return(c(a, mid.pt))
  } else{
    return(c(mid.pt, b))
  }
}


bisect <- function(f, a, b, e){

}