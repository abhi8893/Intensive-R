A <- runif(1e3, -1, 1)
round.A <- round(A, digits = 4)
round.abs.err <- abs(A - round.A)
trunc.A <- trunc((10^4)*A)*(10^(-4))
trunc.abs.err <- abs(A - trunc.A)

df <- data.frame(A, round.A, round.abs.err, trunc.A, trunc.abs.err)
