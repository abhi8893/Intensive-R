# Suppose that the Penn State Committee for the Fun of Students claims 
# that the average number of concerts attended yearly by Penn State 
# students is 2.  Then, suppose that we take a random sample of 50 Penn 
# State students and determine that the average number of concerts 
# attended by the 50 students is 3.2 concerts per year. That then begs 
# the question: if the actual population average is 2, how likely is it that 
# we'd get a sample average as large as 3.2?


# Refer: STAT414: PSU STATISTICS https://t.ly/98jdZ

# Random sample -> X1, X2, X3, X4.....X50
# Xi ~ Binom(n, p)

# Every day the prob of a student going to a concert is p
# There are 365 such trials
n <- 365
# Given: 
# True mean = 2
mu <- 2
# => np = 2, => p=2/n
# H0 probability
p <- mu/n

# Collected data
# Let's for the sake of it, simulate it from some other mean
# NOTE: In function argnames,
#       n = number of collected observations here,
#       size = number of trials
nobs <- 50
d <- rbinom(n=nobs, size=365, p=3.2/n)


# Standardised deviation of the sample mean from the true mean will 
# follow a t-distribution
d.mean <- mean(d)
d.sd <- sd(d)

xbar <- d.mean
sd.Xbar <- d.sd/sqrt(n)

# Now the t-statistic
# For two tailed test, take mod
t.stat <- abs((xbar - mu)/sd.Xbar)

# Critical t.stat at alpha = 0.1 (90%)
# right tail region after critical stat = 0.1/2 = 0.05
# hence to the left region = 1 - 0.05 = 0.95
# TODO: make a plot
alpha <- 0.1
t.stat.critical <- qt(1-(alpha/2), df=nobs-1)

sprintf("Test of significance at %f percent confidence (alpha = %f)",
        (1-alpha)*100, alpha)

if (t.stat > t.stat.critical){
  paste0("Reject H0: the sample did not come from the population ",
          "of individuals with mean=", mu)
} else {
  paste0("Fail to Reject H0: there is not enough evidence that the sample ",
          "came from a different population of individuals with mean=", mu)
}
