# merging dataframes

d1 <- data.frame(kids=c("Jack", "Jill", "Jillian", "John"),
                 states=c("CA", "MA", "MA", "HI"))

d2 <- data.frame(ages=c(10, 7, 12), kids=c("Jill", "Lillian", "Jack"))

# Common entries and automatically merged on common column name (kids)
d <- merge(d1, d2)

d3 <- data.frame(ages=c(12, 10, 7), pals=c("Jack", "Jill", "Lillian"))

# Now we need to "manually" tell on which to merge since the colnames differ
# Auto similiarity index?
merge(d1, d3, by.x="kids", by.y="pals")

# adding a new row
# list because datatypes differ between columns
d2a <- rbind(d2, list(15, "Jill"))
sapply(d2a, class) # BUG: apply gives character as result?

# So rind with an atomic vector will coerce the column to character
d2b <- rbind(d2, c(15, "Jill"))
sapply(d2b, class) 

# Note: The other Jill was assigned the same state instead of NA
merge(d1, d2a)

# But above leaves out other rows
merge(d1, d2a, all = T)


# get sorted columns individually
# NOTE: kids column is a factor variable and was obtained by merging from 
#       a dataframe with more levels so it has the same levels even though the
#       actual entries don't have all of them.
l <- lapply(d, sort)

# WARNING: This will be wrong if you convert l to dataframe
#          as all columns have been sorted individually
as.data.frame(l)
