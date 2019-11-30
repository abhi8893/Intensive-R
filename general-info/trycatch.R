# Catching warnings and errors in R
# Stackoverflow https://t.ly/5Eygb, https://t.ly/gzrvJ

square.ifsmall <- function(i){
  if (i > 10){
    warning(paste("The number,", i, "is too large"))
  } else{
    return(i ** 2)
  }
}

val <- tryCatch(square.ifsmall(11), error=function(e) e)

val <- tryCatch(too.large(11), error=function(e) e, warning=function(w) w)
if (is(val, "warning")){
  print("A warning was encountered")
}

# TODO: How to acces methods/attributes of an object
#       For e.g. w or e is warning and I want to access the message
#       Or in general I need to access the attributes
# Is this the right way?
e <- tryCatch("a" + "b", error = function(e) e)
# list the attributes available for e
attributes(w)
e[["message"]] # or e$message
