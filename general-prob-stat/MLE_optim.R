# Create the log likehood function
# TODO: Make it work for any arbitrary function
# TODO: Make it vectorized

# pdf
# Probability density at x, given parameter alpha
pdf <- function(x, alpha){
  return((1 + alpha*x)/2)
}

# Likelihood of parameter alpha given the data
# You can specify the observed data points and the pdf
# Answers the following question -
#    How likely is the particular value of theta,
#    given that I have observed the following data.

likfunc <- function(theta, x.data, pdf,  take.log=TRUE){
  dens.vals <- pdf(x.data, theta)
  if (take.log){
    sum(log(dens.vals))
  } else{
    prod(dens.vals)
  }
  
}

# Implement NR optimization method

# Derivative 
differentiate <- function(f, x, delta=0.001){
  (f(x+delta) - f(x-delta))/(2*delta)
}

newton.raphson <- function(func, init, eps=0.01){
  moved = 1
  x.now = init
  iter.vals <- c(x.now)
  i=0
  while (moved > eps){
    f.d1 <- function(x) {differentiate(func, x)}
    f.d2 <- function(x) {differentiate(f.d1, x)}
    x.next <- x.now - f.d1(x.now)/f.d2(x.now)
    moved <- abs(x.next - x.now)
    x.now <- x.next
    i = i + 1
    cat("Iteration", i, "=", x.now, '\n')
  }
  
  return (x.now)
}

x.obs <- c(0.4104, 0.9106, -0.6111, 0.3974, 0.3800, 0.3457,
           0.0191, -0.2877, -0.3317, 0.9999, -0.3520, 0.1036,
           0.3057, 0.7528, -0.3374, -0.9146, -0.7622, 0.2715,
           -0.0126, 0.6849, -0.7234, 0.4533, 0.8625, 0.5258,
           0.1415, 0.7665, -0.6554, 0.1250, 0.7497, 0.5384)



alpha.vals <- seq(0, 1, 0.01)
loglik.values <- sapply(alpha.vals, function(alpha) likfunc(alpha, x.obs))
plot(alpha.vals, loglik.values, type='l')
alpha.max <- newton.raphson(function(alpha) likfunc(alpha, x.obs, pdf), 1)
abline(v=alpha.max, col='red')

# Plot the first derivative of loglik function
