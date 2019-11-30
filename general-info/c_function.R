# c function
# concatenate
# since the output is a vector, each element must be of the same type

# NOTE: Every list is a vector
v <- c(1, list(a=1, b=2))
is.vector(v) # TRUE
is.list(v) # TRUE
is.atomic(v) # FALSE

# We can even create a list using vector
vector(mode="list", length=5)

# recursive option in c
c(list(a=1, b=2, c=list(d=5, e=9)))
c(list(a=1, b=2, c=list(d=5, e=9)), recursive = T)

