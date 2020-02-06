# Bayesian Statistics assignment

# Q.1
prior.sprt <- c(0.1, 0.3, 0.5, 0.7, 0.9)
prior.probs <- setNames(rep(1/length(prior.sprt), length(prior.sprt)), 
                            prior.sprt)
# (a) Sample a value of theta from the prior 
theta <- sample(prior.sprt, 1, T, prior.probs)

# (b) Random sample X1, X2...Xn from geometric distr
n <- 12
x.data <- rgeom(n, theta)

# (c) Likelihood function of theta
likfunc <- function(theta, x.data, pdf, take.log = T){
  dens.vals <- pdf(x.data, theta)
  if (take.log){
    sum(log(dens.vals))
  } else{
    prod(dens.vals)
  }
}
loglik.vals <- sapply(prior.sprt, likfunc, x.data=x.data, pdf=dgeom)
names(loglik.vals) <- prior.sprt
# (d) Plot the likelihood function
barplot(loglik.vals, xlab="theta", ylab="log likelihood")
mle <- as.numeric(names(which.max(loglik.vals)))

# posterior
lik.vals <- exp(loglik.vals)
numrtr <- prior.probs*lik.vals
denom <- sum(numrtr)
posterior <- numrtr/denom
# (d) Plot prior and posterior distr
bayes.plot <- function(prior, likelihood, posterior){
  par(mfrow=c(3, 1))
  barplot(prior, main="Prior")
  barplot(likelihood, main="Likelihood")
  barplot(posterior, main="Posterior")
}
bayes.plot(prior.probs, lik.vals, posterior)
# Q2
prior.sprt <- c(0.5, 0.6, 0.9)
num.coins <- c(2, 2, 1)
prior.probs <- setNames(c(num.coins/length(num.coins)),
                        prior.sprt)
x.data <- c(1)
dbern <- function(x, prob){
  ifelse(x == 1, prob, 1 - prob)
}
lik.vals <- sapply(prior.sprt, likfunc, x.data, 
                   dbern, take.log=FALSE)
numrtr <- prior.probs*lik.vals
denom <- sum(numrtr)
posterior <- numrtr/denom

bayes.plot(prior.probs, lik.vals, posterior)

# TODO: BayesBox dataframe

# Q3
rprior <- runif
# Create the observed data as mentioned in the question
x.data <- c(1, 0, 1, 0, 0)
alpha <- 101
beta <- 101
prior.pdf <- function (x) {dbeta(x, alpha, beta)}
curve(prior.pdf, 0, 1, main='Prior')

ngrid <- 1e3
param.grid <- seq(0, 1, length.out = ngrid)

pdf <- function (x) {likfunc(x, x.data, dbern, F)}
lik.vals <- sapply(param.grid, likfunc, x.data, dbern, F)
plot(param.grid, lik.vals, type='l', main='Likelihood')

prior.probs <- sapply(param.grid, prior.pdf)
numrtr <- prior.probs*lik.vals
denom <- sum(numrtr)
posterior <- numrtr

plot(param.grid, posterior, type='l', main='Posterior')


solve.Q3 <- function(n, x, alpha, beta){
  par(mfrow=c(3, 1))
  x.data <- rep(0, n)
  x.data[sample(1:n, x, F)]
  curve(prior.pdf, 0, 1, main='Prior')
  plot(param.grid, lik.vals, type='l', main='Likelihood')
  prior.probs <- sapply(param.grid, prior.pdf)
  numrtr <- prior.probs*lik.vals
  # denom <- sum(numrtr)
  posterior <- numrtr
  
  plot(param.grid, posterior, type='l', main='Posterior')
}
