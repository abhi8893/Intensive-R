# matrix multiplication

sim <- matrix(rnorm(1e5), 1000, 100)

sim.max <- apply(sim, 1, max)
hist(sim.max)

mean(pmax(rnorm(5e4), rnorm(5e4)))
