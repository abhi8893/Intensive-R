# Diff b/w 
# cbind and rbind 
# AND
# cbind.data.frame and rbind.data.frame


v1 <- letters[1:10]
v2 <- 11:20
v3 <- sample(c(T, F), 10, replace=T)

# if cbind doesn't find an argument as data.frame
# it will make a matrix out of it
# and every element of a matrix must be the same
d1 <- cbind(v1, v2, v3)
d1
sapply(d1, class)
# Just for the sake of comparison apply on columns
apply(d1, 2, class)

# change default behaviour of stringsAsFactors to FALSE
options(stringsAsFactors = FALSE)
d2 <- cbind.data.frame(v1, v2, v3)
d2
sapply(d2, class)


# rbind is useful add another record
rbind(d2, list("k", 21, T))

# TODO: Specify column names
rbind.data.frame(list(v1, v2, v3), list("k", 21, T))
