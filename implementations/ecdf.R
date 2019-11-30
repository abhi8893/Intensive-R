# Implement ecdf function
# Ref: Inverse cdf https://t.ly/3Vr52

v <- rnorm(100)

plot(ecdf(v))
abline(h=0.5)

# How to find the median from ecdf
med <- optim(0, function(x) abs(0.5 - ecdf(v)(x)), method = "Brent", 
             lower=-0.1, upper=0.1)$par

abline(v=med)
