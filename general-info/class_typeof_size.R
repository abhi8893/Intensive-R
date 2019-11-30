# General info about class, typeof

v1 <- 1:4
v2 <- c("a", "b", "c", "d")
v3 <- 

class(1)
class(v1)
class(v2)
class(mean)
class(class)


A <- matrix(v1, 2, 2)
class(A)
typeof(A)
typeof(v1)

# Default takes in as a double representation
A <- matrix(c(7, 4, 6, 3), 2, 2)
class(A)
typeof(A)
typeof(v1)


df <- data.frame(v1, v2)
# type of a data.frame is a list! (S3 class)
typeof(df)


# size of an integer is the same as that of double
object.size(1L) == object.size(1.5)
