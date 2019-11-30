# using do.call

v <- 1:10
do.call("[", list(v, 1))

v[5] <- NA_integer_
do.call(mean, list(v, na.rm=T))
