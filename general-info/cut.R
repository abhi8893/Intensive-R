# Cut function

z <- runif(20, 0, 10)
binmarks <- seq(0, 10, 1)

cut(z, binmarks)
cut(z, binmarks, labels = F)

# Also see cutInterval() function
# Ref: Pg137 Art of R Programming
