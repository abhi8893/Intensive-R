# Understanding sort, order and rank
v <- month.name
sort(v)
order(v)
rank(v)


v <- c(8, 9, 7, NA, 9)
# Default behaviour <- ties.method = 'average', na.last = TRUE

rank.methods <- c('average', 'first', 'random', 'max', 'min')

# TODO: How to pass ellipsis ... arguments here?
# VERIFY: that na.last is being passed - OK
l <- lapply(rank.methods, 
            function(t, v, ...) rank(v, ties.method = t, ...), v=v, na.last=T)
names(l) <- rank.methods

as.data.frame(l)

# Sort and order 
identical(v[order(v)], sort(v, na.last = T))

# Rank from 2nd vector in case of ties
# NOTE: Not necessary to create a dataframe
df <- data.frame(v1=c(6, 2, 5, NA, 2, 5, 2),
                 v2=c(673, 201, 523, 7, 199, 501, 200))

order(df$v1, df$v2)

df <- data.frame(kids=c("Jack", "Jill", "Billy"),
                 ages=c(12, 10, 13))
df
# sort by age
df[order(df$ages), ]
# sort by name
df[order(df$kids), ]


# How to rank in decreasing
v <- c(9, 4, 1, 2, 5, NA, 2, 3)
rank(-v)

# Same for character vector
# TODO: How to get in reversed order?
rank(as.numeric(as.factor(month.abb)))

