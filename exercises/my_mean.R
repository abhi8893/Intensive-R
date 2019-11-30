# Custom mean function
# NOTE: Good practice to include na.rm as arg
#       Consistent with R function arg name for removing missing vals
my.mean <- function(v, ..., na.rm = FALSE){
  v <- c(v, ...)
  
  if (na.rm){
    v <- v[!is.na(v)]
  }
  
  return(sum(v)/length(v))
}

v1 <- c(1, 2, NA, 3, 5)
v2 <- c(3, 7, NA, NA, 5)
v3 <- c(NA, NA, 3, 9, 5)

my.mean(v1, na.rm=T)
