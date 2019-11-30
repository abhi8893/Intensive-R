# Multiple replacement of values in a vector
# Stackoverflow https://t.ly/dWr7L

# TODO: Create programmatically
# v <- c(-1, 1, -2, 2, -3, 3, -4, 4, -5, 5, -6, 6)
v <- rep(1:6, each=2)
v[seq_along(v) %% 2 == 1] = -v[seq_along(v) %% 2 == 0]

# Method 1
sapply(v, function(i) ifelse(i<0, 0, 1))
# Method 2


