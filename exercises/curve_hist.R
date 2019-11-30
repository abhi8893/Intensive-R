# Overlay a normal curve on a histogram in R-base
# Ref: https://t.ly/BYyGE

n <- 100
v <- rnorm(n)
h <- hist(v, freq = T)
xfit <- seq(min(v), max(v), length = n)
yfit <- dnorm(xfit, mean = mean(v), sd = sd(v))
yfit <- yfit * diff(h$mids[1:2]) * length(v) 


lines(xfit, yfit, col = 'black', lwd = 2)
