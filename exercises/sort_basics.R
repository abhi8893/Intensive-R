# Sorting a vector

v <- c(1, 4, 3, NA, 5)

# removes NA
sort(v)
# decreasing
# TODO: Partial matching of arg names
sort(v, decr = T)
# NA last value
sort(v, decr = T, na.last = T)
# NA first value
sort(v, decr = T, na.last = F)


# get index of each element based on their sorted order (ascending)
# default behaviour is na.last = T
order(v)
# decreasing order
order(v, decr = T)
# remove NA
order(v, na.last = NA)
# Put NA in the front
order(v, na.last = F)

v <- c(2, 5, 3, 4, NA, 1, 10, NA, 0)
# Descending sort
v[order(v)]
sort(v, decreasing = T, na.last = T)
