# Make a graph of binomial distribution
# Mode = mean for binomial distribution (n is even)
# Mode != mean for binomial distribution (n is odd)
# Refer: Mode of Binomial <- https://t.ly/1nLq7
# Refer: Graph of binomial <- https://t.ly/WMmdv

graph.binom <- function(size, prob){
  x <- dbinom(0:size, size, prob)
  barplot(x, names.arg=0:size, main=sprintf("prob = %.2f", prob))
}

graph.binom(11, 0.5)

# Don't forget to reset to c(1, 1) or get def value from par() list.
# NOTE: You can extract multiple default values in a list from par
#       for final resetting.
def <- list(mfrow=par()$mfrow)
# Is there a sort of with style context manager for this
# i.e. resetting par to default
par(mfrow=c(3,3))

mapply(graph.binom, size=11, prob=seq(0.1, 0.9, 0.1))

# Reset
par(mfrow=def$mfrow)
