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
  accept.samp <- numeric(n)
  accept.dens <- numeric(n)
  reject.samp <- numeric(n)
  reject.dens <- numeric(n)
  i <- 1
  j <- 0
  while(i <= n){
    y <- rg(1)
    u <- runif(1)
    rat <- df(y)/(c*dg(y))
    if (u <= rat){
      accept.samp[i] <- y
      accept.dens[i] <- c*u
      i <- i + 1
    } else{
      reject.samp[i] <- y
      reject.dens[i] <- c*u
    }
    
    j <- j + 1
  }
  l[["accept.samp"]] <- accept.samp
  l[["reject.samp"]] <- reject.samp
  l[["accept.dens"]] <- accept.dens
  l[["reject.dens"]] <- reject.dens
  l[["df"]] <- df
  l[["dg"]] <- dg
  l[["c*dg"]] <- function (x) c*dg(x)
  l[["c"]] <- c
  l[["rg"]] <- rg
  l[["accept.rate"]] <- n/j
  
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
  def.par <- par()
  par(mfrow=c(2, 1),
      cex=0.5)
  
  ylim <- c(0, max(ac$reject.dens))
  # 1st plot
  hist(ac$accept.samp, prob=T,
       xlab = "x",
       ylab = "density",
       main = "Rejection sample", ylim=ylim*1.1, ...)
  df <- ac$df
  curve(df, add=T, col="green")
  
  legend("topright",
         c("True Density"),
         lty=1,
         col="green")
  
  # 2nd plot
  c.dg <- ac$`c*dg`
  plot(ac$accept.samp, ac$accept.dens, col='green', ylim=ylim*1.5)
  points(ac$reject.samp, ac$reject.dens, col='red')
  curve(c.dg, col="blue", add = T)
  df <- ac$df
  curve(df, add=T)
  legend("topright",
         c("Accepted points", "Rejected points", 
           "c*proposal", "target density"),
         col = c("green", "red", "blue", "black"),
         lty = 1)

  # reset par
  suppressWarnings(
    do.call(par, def.par)
  )
  
}


ac <- accept.reject(1000, df, dg, rg)
plot(ac)

# Sample from gamma(3/2, 1)
dgamma <- function(x, a, b){
  c <- b^a/gamma(a)
  return(c * (x^(a-1) * exp(-b*x)))
}
df <- function(x) dgamma(x, 3/2, 1)
dg <- function(x) dexp(x, 1)
rg <- function(n) rexp(n, 1)

ac <- accept.reject(1e3, df, dg, rg, 0, 1000)
plot(ac)
hist(rsamp, prob=T, ylim=c(0, 1))
curve(df, col='red', add=T, xlim=c(0, 10))


# TODO: Implement the accept-reject region plot t.ly/XAb0y
# TODO: Implement the output of accept.reject.sample as an S3 object
#       1. class "accept.reject"
#       2. plot method


c.dg <- ac$`c*dg`
plot(ac$accept.samp, ac$accept.dens, col='green', ylim=c(0, 3))
points(ac$reject.samp, ac$reject.dens, col='red')
curve(c.dg, col="blue", add = T)
df <- ac$df
curve(df, add=T)
legend("topright",
       c("Accepted points", "Rejected points", "c*proposal", "target density"),
       col = c("green", "red", "blue", "black"),
       lty = 1)
