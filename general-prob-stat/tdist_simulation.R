# T random variable - T distribution
# Ref : https://t.ly/EjLE0
n.v <- rnorm(1e4)
r <- 8
chisq.v <- rchisq(1e4, df=r)

t <- n.v/sqrt(chisq.v/r)

hist(t, prob = T, ylim=c(0, 0.4))
curve(dt(x, df=r), -5, 5, add = T)

# TODO: Plot and compare with N(0, 1)
