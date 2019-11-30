# Generate random numbers using inverse cdf
# Ref: https://t.ly/D8K6y

# Generate probabilities between 0 and 1
r <- runif(1000)

# Use the inverse CDF to get the random number corresponding to
# the probability
hist(qnorm(r))
