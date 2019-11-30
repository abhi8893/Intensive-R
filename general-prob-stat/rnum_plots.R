n <- 1e4
size <- 100
prob <- 0.5
smpl <- rbinom(n, 100, 0.5)
h <- hist(smpl, prob = T)
mu <- size*prob
sigma <- sqrt(size*prob*(1-prob))
curve(dnorm(x, mu, sigma), 30, 70, 1e3, add=T, col='red')


plot.distr <- function(rfunc, dfunc, params, n, type){
  
  smpl <- do.call(rfunc, c(list(n=n), params))
  hist(smpl, prob=T)
  expr <- function(x) do.call(dfunc, c(list(x=x), params))
  curve(expr, add=T)
}

plot.distr(rbinom, dbinom, list(size=10, prob=0.5), 100)

p <- dbinom(1:100, size, prob=prob)
h <- table(rbinom(n, size, prob))/size
m <- rbind()
barplot(m, beside = T, width = 0.1, space = 5, xlim = c(30, 70), 
        col = c("red", "blue"))
