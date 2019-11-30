
# oddcount function
oddcount <- function(v){
  k <- 0
  for (x in v){
    if (x %% 2 == 1) k <- k + 1
  }
  return(k)
}

# Valid in R but not recommended
f <- function(x=2, y){
  x + y
}

# character string is NOT an iterable
for (i in "abc"){print(i)}

# Split a string in R
strsplit("a b c", sep=" ")

# TODO: split using ""
# TODO: Get errors 
tryCatch(strsplit("abc"), error=function(e) e)

# Did you know even operators are also a function?
"+"(2, 3)

# Excluding between ranges
v <- 1:10
v[-1:-3]

# colon ":" operator takes precedence over mathematical operators (+,-,/, etc)
# NOTE: Access help on operators by ?Syntax
v <- 1:10
v[1+2:5]
v[(1+2):5]

# Better alternative
# Bad
1:10
# Good
seq(10)