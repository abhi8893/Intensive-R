# Chi sq using simulation (sample variance)
# Ref: https://t.ly/KJWwG
sample.size <- 8
num.samples <- 1e4
mu <- 100
sigma <- 16
d <- rnorm(sample.size*num.samples, mu, sigma)
dim(d) <- c(num.samples, sample.size)

# sample variance for each sample
s2 <- apply(d, 1, var)

# (n-1)*s^2/(sigma^2)
# y ~ chisq(df=7)
y <- s2*(sample.size - 1)/(sigma**2)

# Plot it!
hist(y, prob=TRUE)
curve(dchisq(x, df=7), 0, 25, add = TRUE)

# TODO: Fit different distributions using MLE and compare likelihood