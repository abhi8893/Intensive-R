# Accept-Reject method to sample from a distribution

# Sample from a Beta(2, 4) distribution


dbeta <- function(x, a, b){
  c <- gamma(a+b)/(gamma(a)*gamma(b))
  return(c * (x^(a-1)) * ((1-x)^(b-1)))
}

df <- function(x) dbeta(x, 2, 4)

# Choose the proposal distribution which has the same support
dg <- function(x) dunif(x, 0, 1)
rg <- function (n) runif(n, 0, 1)

curve(df)
curve(dg, add=T)

# Now choose a c such that [f(x)/g(x)] is maximised
# which implies I have to minimize g(x)/f(x)
h <- function(x) df(x)/dg(x)
x.max <- optim(0, function (x) 1/h(x), 
               method = "Brent", lower = 0, upper = 1)$par
curve(h)
abline(v=x.max, col="red")

find.C <- function(df, dg){
  h <- function(x) df(x)/dg(x)
  x.max <- optim(0, function (x) 1/h(x), 
                 method = "Brent", lower = 0, upper = 1)$par
  
  return(df(x.max)/dg(x.max))
}

accept.reject <- function(n, df, dg, rg){
  l <- list()
  
  c <- find.C(df, dg)
  rsamp <- numeric(n)
  i <- 1
  while(i <= n){
    y <- rg(1)
    u <- runif(1)
    rat <- df(y)/(c*dg(y))
    if (u <= rat){
      rsamp[i] <- y
      i <- i + 1
    }
  }
  l[["rsamp"]] <- rsamp
  l[["df"]] <- df
  l[["dg"]] <- dg
  l[["rg"]] <- rg
  
  class(l) <- "accept.reject"
  return(l)
}
# sample - not a generic function?
sample.accept.reject <- function(ac, n){
  # Maybe use do.call?
  return(accept.reject(n, ac$df, ac$dg, ac$rg))
}

# ... ellipsis specific to a certain function?
# Maybe use a try except with all functions?
# But a subset of some arguments may be valid for
# for a particular function.
plot.accept.reject <- function(ac, ...){
  hist(ac$rsamp, prob=T,
       xlab = "x",
       ylab = "density",
       main = "Rejection sample", ...)
  df <- ac$df
  curve(df, add=T, col="red")
  
  legend("topright",
         c("True Density"),
         lty=1,
         col="red")
}


ac <- accept.reject.sample(1e4, df, dg, rg)
hist(ac$rsamp, prob=T, ylim=c(0, 1))
curve(ac$df, col='red', add=T)

# Sample from gamma(3/2, 1)
dgamma <- function(x, a, b){
  c <- b^a/gamma(a)
  return(c * (x^(a-1) * exp(-b*x)))
}
df <- function(x) dgamma(x, 3/2, 1)
dg <- function(x) dexp(x, 1)
rg <- function(n) rexp(n, 1)

rsamp <- accept.reject.sample(1e5, df, dg, rg)$rsamp
hist(rsamp, prob=T, ylim=c(0, 1))
curve(df, col='red', add=T, xlim=c(0, 10))


# TODO: Implement the accept-reject region plot t.ly/XAb0y
# TODO: Implement the output of accept.reject.sample as an S3 object
#       1. class "accept.reject"
#       2. plot method