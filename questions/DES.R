# Discrete Event Simulation
# Ref: Pg 164 Art of R Programming

theta.arr <- 2
interarr.dur <- rexp(1e3, rate = 1/theta.arr)
theta.srvc <- 1
srvc.dur <- rexp(1e3, rate = 1/theta.srvc)

arr.time <- cumsum(interarr.dur)
dep.time <- arr.time + srvc.dur

atm <- data.frame(arr.time, dep.time, srvc.dur)
