# sweep function for matrices

m <- matrix(1:9, 3, 3, byrow = T)
m

# MARGIN confusion
# NOTE: "+" is just a function and 
# c(1, 4, 7) is the 2nd arg to the function
# 1: add to columns
sweep(m, 1, c(1, 4, 7), "+")

# 2: add to rows
sweep(m, 2, c(1, 4, 7), "+")

