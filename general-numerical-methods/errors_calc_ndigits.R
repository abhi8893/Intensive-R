# Errors

is.floatnum <- function(n){
  s <- as.character(n)
  return("." %in% s)
}

num.digits <- function(n){
  ndig <- as.integer(log10(n) + 1)
  return(ndig)
}

num.digits <- function(n){
  s <- as.character(n)
  ndig <- nchar(s)
  if ("." %in% s){
    ndig <- ndig - 1
  }
  return(ndig)
}

extract.ndigits <- function(n, ndig){
  s <- as.character(n)
  if (is.floatnum(n)){
    ndig <- ndig + 1
  }
  s.ndig <- strsplit(s, NULL)[[1]][1:ndig]
  n.ndig <- as.numeric(s.ndig)
  return(n.ndig)
}

num.afterdecimal <- function(n){
}


display.calc <- function(n){
}

calc.process <- function(n, max.dig = 4){
  if (n  > 9999){
    stop("ValueError: Number outside the range!")
  }
  
  log.n <- log10(n)
  
  # Number is between 0 and 1
  if (log.n < 0){
    return(round(n, max.dig - 1))
  }
  
  ndig <- as.integer(log.n + 1)
  return(round(n, max.dig - ndig))
}


f1 <- function(x){
  val <- x*(sqrt(x+1) - sqrt(x))
  return(val)
}

f1.calc <- function(x){
  cp <- calc.process
  val <- cp(x)*(cp(sqrt(x+1)) - cp(sqrt(x)))
  return(cp(val))
}

f2 <- function(x){
  val <- x/(sqrt(x+1) + sqrt(x))
  return(val)
}
f2.calc <- function(x){
  cp <- calc.process
  val <- cp(x)/(cp(sqrt(x+1)) + cp(sqrt(x)))
  return(cp(val))
}



v <- sort(runif(1e3, 0, 1e3))
# v <- c(1, 10, 100, 1000, 9999)

d <- data.frame(
  n=v,
  "f1(x)"=sapply(v, f1.calc),
  "Actual f1(x)"=f1(v),
  "f2(x)"=sapply(v, f2.calc),
  "Actual f2(x)"=f2(v),
  check.names = F
  )

d[, "f1.abserror"] <- abs(d$`f1(x)` - d$`Actual f1(x)`)
d[, "f2.abserror"] <- abs(d$`f2(x)` - d$`Actual f2(x)`)

plot(c(0, 1000), c(3, 20), type = "n")
lines(d$n, d$`Actual f1(x)`, type="l", col="black", lty=1)
lines(d$n, d$`f1(x)`, col="red")

lines(d$n, d$`Actual f2(x)`, type="l", col="black", lty=2)
lines(d$n, d$`f2(x)`, col="blue")

legend("topleft",
       legend=c("Actual f1(x)",
                "Actual f2(x)",
                "f1(x)",
                "f2(x)"),
       col=c("black", "black", "red", "blue"),
       lty=c(1, 2, 1, 1))


plot(d$n, d$f1.abserror, type="l", col = "red", xlab = "x", 
     ylab = "Evaluated Function value", main = "Absolute error")
lines(d$n, d$f2.abserror, type="l", col = "blue")
legend("topleft",
       c("f1(x)", "f2(x)"),
       lty=1,
       col=c("red", "blue"))
