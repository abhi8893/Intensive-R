# Properly check if a value is TRUE or FALSE

x <- 1

# WRONG 
if (x == TRUE){
  cat("executing the if block since x is TRUE")
}

if (isTRUE(x)){
  cat("executing the if block since x is TRUE")
} else {
  cat("executing else block since x is NOT TRUE")
}
