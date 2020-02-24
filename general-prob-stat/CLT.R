
sample.size <- 100
num.sims <- 1e4
sample.means <- vector("numeric", num.sims)
binom.params <- list(size=50, prob=0.2)
for (i in 1:num.sims){
  rand.samp <- rbinom(sample.size, binom.params$size, binom.params$prob)
  sample.means[i] <- mean(rand.samp)
}

n <- binom.params$size
p <- binom.params$prob

pop.mu <- n*p
pop.var <- n*p*(1-p)
sample.means.stdzd <- (sample.means - pop.mu)/(sqrt(pop.var/num.sims))
hist(sample.means.stdzd, probability = T)
curve(dnorm, -10, 10, add=T)
