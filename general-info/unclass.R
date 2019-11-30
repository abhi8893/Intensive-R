# Unclass

vote.for.x <- sample(c("yes", "no", "maybe"), 1e4, replace = T)
voted.for.x.last.time <- sample(c("yes", "no"), 1e4, replace = T)

df <- data.frame(vote.for.x, voted.for.x.last.time)

tbl <- table(df)

class(tbl)

class(unclass(tbl))
