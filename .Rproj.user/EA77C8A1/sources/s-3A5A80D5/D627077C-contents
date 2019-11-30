# Some general simulations with die, coins, cards

# Problem
# TODO: Modify to have biased coin and die.
# How many six in die roll when there is a head?
# head <- 1, tails <- 0

coin.vals <- c(0, 1)
die.vals <- 1:6
coins <- sample(coin.vals, 1e3, replace = T)
dies <- sample(die.vals, 1e3, replace = T)

# How many heads did we get?
sum(dies[coins == 1] == 6)


# Repeat above experiment 1000 times
n <- 1e3
res <- vector(1e3, mode="numeric")
for (i in 1:n){
  coins <- sample(coin.vals, 1e3, replace = T)
  dies <- sample(die.vals, 1e3, replace = T)
  res[i] <- sum(dies[coins == 1] == 6)
}

# Plot the variation in number of 6 obtained in each identical experiment
# sampling variation <- Normal distribution
hist(res, main="number of 6 when there is a head", probability = T)
curve(dnorm(x, mean=mean(res), sd=sd(res)), 60, 110, add = T, col = "red")

