# Accept.reject functions

find.C <- function(df, dg, lower, upper){
  h <- function(x) df(x)/dg(x)
  x.max <- optim(0, function (x) 1/h(x), 
                 method = "Brent", 
                 lower = lower, upper = upper)$par
  
  return(df(x.max)/dg(x.max))
}

# TODO: Make it vectorized
accept.reject <- function(n, df, dg, rg, lower, upper){
  l <- list()
  
  c <- find.C(df, dg, lower, upper)
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
      accept.dens[i] <- u*c*dg(y)
      i <- i + 1
    } else{
      reject.samp[i] <- y
      reject.dens[i] <- u*c*dg(y)
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
       main = "Rejection sample", ...)
  df <- ac$df
  curve(df, add=T, col="green")
  
  legend("topright",
         c("True Density"),
         lty=1,
         col="green")
  
  # 2nd plot
  c.dg <- ac$`c*dg`
  plot(ac$accept.samp, ac$accept.dens, col='green', ylim=ylim*1.5,
       xlab="x", ylab="density")
  points(ac$reject.samp, ac$reject.dens, col='red')
  curve(c.dg, col="blue", add = T)
  df <- ac$df
  curve(df, add=T)
  legend("topright",
         c("Accepted points", "Rejected points", 
           "c*proposal", "target density"),
         col = c("green", "red", "blue", "black"),
         lty = 1)
  
  # reset par (BUG: Doesn't work)
  # suppressWarnings(
  #   do.call(par, def.par)
  # )
  
  par(mfrow=c(1, 1), cex=1)
  
}


accept.reject.vectorized <- function(n, df, dg, rg, lower, upper){
  l <- list()
  
  c <- find.C(df, dg, lower, upper)
  accept.samp <- numeric(n)
  accept.dens <- numeric(n)
  reject.samp <- numeric(n)
  reject.dens <- numeric(n)
  
  m <- n*c*2
  y <- rg(m)
  u <- runif(m)
  rat <- df(y)/(c*dg(y))
  
  accept.status <- numeric(m)
  accept.status[u<=rat] <- 1
  accept.status[u>rat] <- 0
  dens <- u*c*dg(y)
  
  l[["accept.samp"]] <- y[accept.status == 1]
  l[["accept.dens"]] <- dens[accept.status == 1]
  l[["reject.samp"]] <- y[accept.status == 0]
  l[["reject.dens"]] <- dens[accept.status == 0]
  l[["df"]] <- df
  l[["dg"]] <- dg
  l[["c*dg"]] <- function (x) c*dg(x)
  l[["c"]] <- c
  l[["rg"]] <- rg
  l[["accept.rate"]] <- mean(accept.status)
  
  class(l) <- "accept.reject"
  return(l)
}
