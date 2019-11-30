# Implement cut function

z <- runif(20, 0, 10)
binmarks <- seq(0, 10, 1)

cut(z, binmarks)
cut(z, binmarks, labels = F)

my.cut <- function(v, b, labels = T){
  
}