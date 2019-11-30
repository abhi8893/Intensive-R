# rbind data frames with a different column name
# Ref: Stackoverflow https://t.ly/lYAVk
d <- data.frame(a=1:10, b=letters[1:10])
l1 <- list(a=c(11, 12),  b=c("k", "l"))
l2 <- list(c=c(11, 12),  d=c("k", "l"))

# This is allowed
rbind(d, l1)

# This is not allowed, as column names differ
rbind(d, l2)

# This works but coerces datatypes to same [character in current case]
mapply(c, d, l2)


a <- data.frame(a=letters[1:5], b=runif(5))
b <- data.frame(c=letters[6:10], d=runif(5))

# Error!
rbind(a, b)

# Coerces datatype to character!
mapply(c, a, b)

# This would work
rbind(a, setNames(b, names(a)))
