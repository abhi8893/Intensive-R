# Computer intensive Statistical Methods
# 3rd March 2019
# Exercises

source("accept-reject-functions.R")
source("inverse_transform_functions.R")

# Q.1 
df <- function(x){
  exp(x)/( exp(1) - 1)
}
dg <- dunif
rg <- runif

ac <- accept.reject(1000, df, dg, rg, 0, 1)
plot(ac)

# Q.2 
df <- function(x){
  ifelse(x>=2 & x<= 3,
         (x-2)/2,
         ifelse( x>3 & x<=6, (2 - x/3)/2, 0))
}

rg <- function(x) runif(x, 2, 6)
dg <- function(x) dunif(x, 2, 6)

ac <- accept.reject(1000, df, dg, rg, 2, 6)
plot(ac)

# Q3
inv.func <- function(x){
  ifelse(x>=0 & x<=1, (sqrt(1+8*x) - 1)/2, 0)
}
pdf <- function(x){x + 1/2}
inv.transform(1e4, inv.func, pdf = pdf)

# Q4
a <- 1
b <- 4
inv.func <- function(x){
  ifelse(x>0, (-log(1-x)/a)**(1/b), 0)
}

pdf <- function(x){a*b*(x**(b-1))*exp(-a*(x**(b)))}
inv.transform(1e4, inv.func, pdf=pdf)

# Q6
# (i)
dg <- function(x) ifelse(x>=0, dexp(x, 1))
rg <- function(n) rexp(n, 1)
 
ac <- accept.reject(1e3, dnorm, dg, rg, lower = 0, 10)
plot(ac)

# (ii)
dg <- function(x) dcauchy(x, 0, 1)
rg <- function(n) rcauchy(n, 0, 1)
ac <- accept.reject(1e3, dnorm, dg, rg, lower = 0, upper = 10)
plot(ac)

# Q7
df <- function (x) {ifelse(x>0 & x<1, (x*(1-x)*exp(x))/(3-exp(1)), 0)}
ac <- accept.reject(1e3, df, dunif, runif, 0.1, 2)
plot(ac)
