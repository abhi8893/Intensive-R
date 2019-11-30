# function info

g <- function(x, y){
  return(x+y)
}

f <- formals(g)
f
class(f)
b <- body(g)
b
class(b)

# get any function code
abline

# open in a page
page(abline)

# some functions are written in C so can't be viewed directly
sum

# change the body of a function
g <- function(x) x^2
g(2)
body(g)
body(g) <- quote(2*x+3)
body(g)
g(2)
