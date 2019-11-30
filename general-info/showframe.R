# showframe function

showframe <- function(upn){
  if (upn < 0){
    env <- .GlobalEnv
  } else {
    env <- parent.frame(n=upn+1)
  }
  
  vars <- ls(envir = env)
  
  for (vr in vars){
    vrg <- get(vr, envir = env)
    
    cat(vr, " : ", class(vrg), "\n", sep="")
    
    if (!is.function(vrg)){
      print(vrg)
      }
  }
}

h <- function(aaa){
  c <- 3
  return(aaa+c)
}

g <- function(aa){
  b <- 2
  showframe(0)
  showframe(1)
  aab <- h(aa+b)
  return(aab)
}

f <- function(){
  a <- 1
  k <- function(x){
    return(x^2)
  }
  return(g(a) + k(a))
}

f()
