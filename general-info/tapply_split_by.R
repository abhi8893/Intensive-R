# Operations with group (factor) using tapply

ages <- c(25, 26, 55, 37, 21, 42)
affils <- c("R", "D", "D", "R", "U", "D")
# tapply(vector, index, function)
# basically we are passing index of each element
# tapply temporarily separarates them into groups 
# and applies FUN

tapply(ages, affils, mean)


# More than one factor
d <- data.frame(gender=c('M', 'M', 'F', 'M', 'F', 'F'),
                age=c(47, 59, 21, 32, 33, 24),
                income=c(55000, 88000, 32450, 76500, 123000, 45650))
d[, "over25"] <- ifelse(d$age > 25, "yes", "no")
d

m <- tapply(d$income, as.list(subset(d, select=c(gender, over25))), mean)
m
class(m)

# The row and column header names are names of dimnames
names(dimnames(m))

# Remove them
names(dimnames(m)) <- NULL
m

# Find number of entries corresponding to each factor
# NOTE: NA in case of no entries
tapply(d$income, as.list(subset(d, select=c(gender, over25))), length)

# NOTE: Above didn't actually make use of the income value
# We can use table to make a proper contingency table
table(as.list(subset(d, select=c(gender, over25))))



# Splitting groups using split()
split(ages, affils)

# You can split dataframes too into a list!
split(d, subset(d, select=c(gender, over25)))

# Get indices of each group
g <- c("M", "F", "F", "I", "M", "I", "F", "M")
split(seq_along(g), g)

