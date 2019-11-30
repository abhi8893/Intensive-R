# Recycling

m <- matrix(vector(mode="numeric", length=6), 3, 2)
# Adds it column wise by recycling
# Internally c(1, 2) was changed to the same dim as m 
# filling column wise as its default behaviour and then added
m + c(1, 2)
