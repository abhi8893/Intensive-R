# Flatten a matrix and unflatten it
# Ref: stat.eth https://t.ly/A6BbO
# IDEA: Contextmanager implementation?
# Flatten => give control of object => unflatten
# Ref: Contextmanager in R https://t.ly/28D1q

m <- matrix(1:9, 3, 3)
m.dims <- dim(m)
dim(m) <- NULL
subset(m, m>5)
dim(m) <- m.dims
