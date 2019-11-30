# Plotting general info

# you can plot like functions like this
plot(sin, 0, 4*pi, col="blue")
plot(function(x) sin(2*x), 0, 4*pi, add=T, col="red")

# Empty graph
# type = "n" means there is nothing in the graph
plot(c(-3, 3), c(-1, 5), type = "n", xlab = "x", ylab = "y")


# Plot linreg line
x <- 1:100
y <- 1 + 2*x + rnorm(length(x), sd=5)
plot(x, y)
lmout <- lm(y~x)
# Calls abline on lmout$coefficients
abline(lmout, col = "red")
abline(c(1, 2), col = "green")

# How to specify topright, bottomleft etc as pos in text?
text(75, 25,
     "Errors are assumed to be Normally distributed",
     cex = 0.8)
legend("topleft",
       legend = c("fitted line",
                  "True population relationship"),
       col = c("red", "green"))


plot(c(-3, 3), c(-1, 5), type = "n", xlab = "x", ylab = "y")
# Draw any line 
lines(c(-1, 1), c(0, 4))

plot(c(-1, 1), c(1, 5), type = "l")
lines(c(-1, 1), c(0, 4), col = "red")
