# Getting argument names in ...  

get.argnames <- function(...){
  # BUG: when called like get.argnames(v1, v2) names are not parsed
  
  l <- as.list(substitute(list(...)))[-1L]
  if (is.null(names(l))){
    # Converting elements of list
    # from mode "name" to "character"
    # to allow converting to a vector
    # when unlist is called
    names <- unlist(lapply(l, as.character))
  } else{
    names <- names(l)
  }
  
  return(names)
}

get.arglist <- function(...){
  argnames <- get.argnames(...)
  l <- list(...)
  names(l) <- argnames
  return(l)
}

get.argnames(v1, v2)

