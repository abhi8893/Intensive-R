# Replace vectors divisible by 2 -> 0, 3 -> 1, both -> 2
# TODO: Stackoverflow https://t.ly/7v9K2


my.mapvalues <- function(v, values, replace.with){
  lookup <- replace.with
  names(lookup) <- values
  
  func <- function(x){
    rep.val <- lookup[as.character(x)]
    names(rep.val) <- NULL
    if (is.na(rep.val)){
      return(x)
    } else{
      return(rep.val)
    }
  }
  return(sapply(v, func, USE.NAMES = F))
}

if (!interactive()){
  v1 <- 1:10
  my.mapvalues(v1, c(1, 5, 9), c(13, 25, 16))
}
