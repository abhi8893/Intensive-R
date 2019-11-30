# Unpack list arguments into a function

f <- function(x, y) x - y

do.call(f, list(x=2, y=1))
do.call(f, list(y=1, x=2))
