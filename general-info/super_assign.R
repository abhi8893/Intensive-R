# Writing to Nonlocals with the Superassignment Operator
# Superassignment Operator : <<-
p <- 2
raise.to <- function(x){
  p <<- p + 1 # Modify the Nonlocal variable
  return(p^x)
}

raise.to(2)
p

p <- 2
raise.to <- function(x){
  p <- p + 1 # will create a new local variable as it only has read access
             # with the assignment operator
  return(p^x)
}

raise.to(2)
p

make.var <- function(val, name){
  assign(name, val, pos = .GlobalEnv)
}

make.var(1, "my.var")
my.var

