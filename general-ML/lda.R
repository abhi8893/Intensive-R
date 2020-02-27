# Load the dataframe
df <- subset(iris, Species %in% c("setosa", "virginica"))[, c(1, 2, 5)]
# setosa = 1, virginica = 3
df[, 3] <- as.factor(as.numeric(df[, 3]))
X <- as.matrix(df[, 1:2])


within.class.scatter <- function(X){
  mu <- as.numeric(colMeans(X))
  Sw <- 0
  n <- nrow(X)
  for (i in 1:n){
    x_mu <- as.numeric(X[i, ] - mu)
    Sw <- Sw + x_mu %*% t(x_mu)
  }
  
  return(Sw)
}

# Can also calculate Sw in this way
# X1 <- X[df$Species == 1 ,]
# X2 <- X[df$Species == 3 ,]
# Sw <- cov(X1)*(nrow(X1) -1) + cov(X2)*(nrow(X2) -1)

species <- unique(df[, "Species"])

Sw <- 0
means <- list()
for (sp in species){
  s <- subset(df, Species == sp)[, -3]
  means[[sp]] <- as.numeric(colMeans(s))
  Sw <- Sw + within.class.scatter(s)
}

diff.means <- matrix((means[[1]] - means[[2]]), 4, 1)
v <- solve(Sw)%*% (means[[1]] - means[[2]])
# Sw2 <- as.matrix(cov(s)*(n-1))
# all(Sw == Sw2)


# Project the points
X.lda <- X %*% v
colors <- c("red", "blue")[as.numeric(df[, 3])]
# stripchart(X.lda, col = colors)



plot(X.lda, rep(0, length(X.lda)), col= colors, ylim = c(0, 20))
r <- X.lda[colors == "red", ]
l <- X.lda[colors == "blue", ]
# dcsn.bndry <- (min(r) + max(l))/2
# abline(v=dcsn.bndry)

# l <- l + 1
# r <- r - 0.5

mu.l <- mean(l)
sd.l <- sd(l)

mu.r <- mean(r)
sd.r <- sd(r)

d1 <- function(x){
  dnorm(x, mu.l, sd.l)
}

d2 <- function(x){
  dnorm(x, mu.r, sd.r)
}

curve(d1, add=T, mu.l - 3*sd.l, mu.l + 3*sd.l , col="blue")
curve(d2, add=T, mu.r - 3*sd.r, mu.r + 3*sd.r , col="red")

dcsn.bndry <- uniroot(function (x) d1(x) - d2(x), interval = c(-0.5, 0))$root
abline(v=dcsn.bndry)

plot(X, col=colors)
a <- dcsn.bndry/v[2]
b <- -v[1]/v[2]
abline(a, b)

# Probability of training error
l.err <- 1 - pnorm(dcsn.bndry, mu.l, sd.l)
r.err <- pnorm(dcsn.bndry, mu.r, sd.r)
train.errprob <- l.err + r.err
train.errprob


# test point
p <- matrix(c(5.75, 3.6), 2, 1)
p.v <- t(v) %*% p

if (p.v < dcsn.bndry){
  pred <- "setosa"
} else{
  pred <- "virginica"
}

