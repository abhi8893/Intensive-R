# Nested functions

f <- function(y){
  return(y^2)
}

# Since f is created at top-level
environment(f)

# See the variables in .GlobalEnv
ls()
ls.str()

w <- 1
f <- function(y){
  d <- 8
  h <- function(){
    return(d*(w+y))
  }
  
  print(environment(h))
  return(h())
}

f(5)

f <- function(y, ftn){
  d <- 8
  print(environment(ftn))
  return(ftn(d, y))
}

h <- function(a, b){
  return(a + w*b)
}

# since ftn now will refer to h, it will have the R_GlobalEnv, 
# even though it's a local variable
f(2, h)

f <- function(y, n){
  d <- 8
  return(h(d, y, n))
}

h <- function(a, b, n=1){
  print(ls())
  print(ls(envir = parent.frame(n=n)))
  return(a + b*w)
}

# parent.frame(), the argument n specifies how many frames to go
# up in the call chain.

# n=1 will go in local frame: f()'s frame
f(2, n=1)
# n=2 will go up the R_GlobalEnv
f(2, n=2)


