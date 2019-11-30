# Plot using mapply
# TODO: How to use LaTeX in this?
# TODO: Make a proper generalised function 

v1 <- rnorm(100, mean=10, sd=4)
v2 <- rnorm(100, mean=10, sd=2)
v3 <- rnorm(100, mean=10, sd=1)
# TODO: Write main text using paste or some variable substitution
plot(v1, type="l", main="Normal distribution with mean=100 and sd = 4, 2, 1", 
     xlab = "Index of generated numbers", ylab = "Value", col="red")
lines(v2, col="green")
lines(v3, col="blue")
# TODO: Relative location? 
legend(80, 7, paste("SD = ", c(10, 1, 0.1)), lty=c(1, 1, 1),
       col=c("red", "green", "blue"))

# TODO: locator
legend(locator(1), paste("SD = ", c(10, 1, 0.1)), lty=c(1, 1, 1),
       col=c("red", "green", "blue"))

# xlim and ylim
plot(v1, type="l", main="Normal distribution with mean=100 and sd = 4, 2, 1", 
     xlab = "Index of generated numbers", ylab = "Value", col="red")


