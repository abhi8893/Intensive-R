# do.call with arguments

l <- list(a="1", b="2")

do.call(paste, c(l, list(sep=", ")))

# Or put it into a single list
l <- list(a="1", b="2", sep=", ")
do.call(paste, l)
