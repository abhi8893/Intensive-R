#' ---
#' title: Assignment 2 (Machine Learning)
#' author: Abhishek Bhatia
#' date: "27th February 2019"
#' ---
#' Assignment 2 <br/>

#' Use the data.csv file for two class two feature problem 1373 data points.
#' Seperate the first 100 data points from each of the classes (0 and 1) for 
#' testing and the remaining data points for training. <br/>

#' Load the data
df <- read.csv('data/data.csv')
head(df)

test.rows <- 1:100
test <- Reduce(rbind, lapply(c(0, 1), 
                             function (i) subset(df, class == i)[test.rows, ]))
train.rows <- -as.numeric(rownames(test))
train <- df[train.rows, ]

#' Make the data matrix
X <- as.matrix(train[, 1:2])

#' Within class scatter function

within.class.scatter <- function(X, method=c('slow', 'fast')){
  method = match.call()
  if (method == 'fast') return (cov(X)*(nrow(X) -1))
  
  mu <- as.numeric(colMeans(X))
  Sw <- 0
  n <- nrow(X)
  for (i in 1:n){
    x_mu <- as.numeric(X[i, ] - mu)
    Sw <- Sw + x_mu %*% t(x_mu)
  }
  
  return(Sw)
}

#' 1. Report the mean and scatter of both the classes. [4]
# Get the unique classes
y.uniq <- unique(df[, "class"])

# This list will store the results
res <- list(mean=list(), scatter=list())
Sw <- 0
for (lab in y.uniq){
  s <- subset(df, class == lab)[, -3]
  lab <- as.character(lab)
  res$mean[[lab]] <- matrix(as.numeric(colMeans(s)), 2, 1)
  res$scatter[[lab]] <- within.class.scatter(s, "fast")
  
  Sw <- Sw + res$scatter[[lab]]
}

res

#' 2. Find the direction (v) which maximizes the separation of the 
#' classes using LDA. [2]

diff.means <- res$mean[["0"]] - res$mean[["1"]]
v <- solve(Sw) %*% (diff.means)

#' 3. Project all  training data points onto v and create your own decision rule. 
#' Write as comment in your code about the decision criterion. In the overlapping 
#' region on the projected line you can randomly predict the classes. [8]
X.lda <- X %*% v
colors <- character(length(train[, 3]))
colors[train[, 3] == 0] <- "red"
colors[train[, 3] == 1] <- "blue"

colors <- c("red", "blue")[train[, 3] + 1]

plot(X.lda, rep(0, length(X.lda)), col= colors, yaxt="n", ylab="",
     main="LDA axes projected data points",
     ylim=c(0, 10))
#' Slightly shifted version to clearly see the overlapping points
plot(X.lda, train[, 3], col= colors, ylim = c(0, 20),
     main="LDA axes projected data points", ylab="", yaxt="n")
legend("topright", 
       paste("class", y.uniq), 
       col=c("red", "blue"), 
       pch=1)

r <- X.lda[colors == "red", ]
l <- X.lda[colors == "blue", ]
dcsn.bndrs <- c(min(r), max(l))
# plot the decision boundaries
abline(v=dcsn.bndrs)
#' The rule for the decision:
paste('if the the coordinate of the projected point on the lda is',
    'less than', dcsn.bndrs[1], "predict = 1")

cat('if the the coordinate of the projected point on the lda is',
    'greater than', dcsn.bndrs[2], "predict = 0")

cat('Else randomly predict 0 or 1 with equal probability')

pred.func <- function(x){
  ifelse(x < dcsn.bndrs[1], 1, 
         ifelse(x > dcsn.bndrs[2], 0, 
                sample(y.uniq)))
  
}

# Make the test and train subsets
X.train.lda <- X.lda
X.test <- as.matrix(test[, 1:2])
X.test.lda <- X.test %*% v
cl <- train[, 3]
test.true <- test[, 3]

# Accuracy function
accuracy <- function(y.true, y.pred){
  return(mean(y.true == y.pred))
}

# Evaluate the crude decision criteria
test.preds.crude <- pred.func(X.test.lda)
cnf.mat <- table(test.preds.crude, test.true)
acc.crude <- accuracy(test.true, test.preds.crude)
paste("The accuracy for the crude decision criteria is", acc.crude)

#' How does the Discriminant axes look like in the 2D scatter plot ?
plot(X, col=colors, main="LDA axes")
abline(a=0, b=v[2]/v[1])

#' How does the crude decision boundary look in the 2D scatter plot?
draw.lda.pt <- function(x, col){
  a <- x/v[2]
  b <- -v[1]/v[2]
  abline(a, b, col=col)
}
plot(X, col=colors, main="Crude Decision Boundaries")
draw.lda.pt(dcsn.bndrs[1], col="blue")
draw.lda.pt(dcsn.bndrs[2], col="red")
legend("topright", 
       paste("class", y.uniq), 
       col=c("red", "blue"), 
       pch=1)

#' 5. Perform KNN on the projected data points for k=5 to predict the classes of the 200 test points. 
#' Report the accuracy with confusion matrix.[2]



library(class)

k <- 5
test.preds.knn <- knn(X.train.lda, X.test.lda, cl, k = k)
cnf.mat <- table(test.preds.knn, test.true)
cnf.mat
acc.knn <- accuracy(test.true, test.preds.knn)
paste("The accuracy for knn with k =",  k, "is", acc.knn)


#' Would modelling them as gaussians help?
n.l <- length(l)
n.r <- length(r)

mu.l <- mean(l)
sd.l <- sd(l)
prior.l <- n.l/(n.l + n.r)

mu.r <- mean(r)
sd.r <- sd(r)
prior.r <- n.r/(n.l + n.r)

d1 <- function(x){
  dnorm(x, mu.l, sd.l)*prior.l
}

d2 <- function(x){
  dnorm(x, mu.r, sd.r)*prior.r
}
ylim.max <- max(d1(0), d2(0))*1.2
plot(X.lda, rep(0, length(X.lda)), col= colors, ylim = c(0, ylim.max),
     ylab="Posterior density", main="Gaussian NaiveBayes on LDA axes")
legend("topright", 
       paste("class", y.uniq), 
       col=c("red", "blue"), 
       pch=1)



curve(d1, add=T, mu.l - 3*sd.l, mu.l + 3*sd.l , col="blue")
curve(d2, add=T, mu.r - 3*sd.r, mu.r + 3*sd.r , col="red")

# Find and plot the decision boundary
dcsn.bndry <- uniroot(function (x) d1(x) - d2(x), interval = c(0, 0.002), 
                      tol = 1e-5)$root
abline(v=dcsn.bndry)

# How does this decision boundary look in the 2 dimensions?
plot(X, col=colors, main="LDA projected Gaussian Naive Bayes Decision boundary")
a <- dcsn.bndry/v[2]
b <- -v[1]/v[2]
abline(a, b)
legend("topright", 
       paste("class", y.uniq), 
       col=c("red", "blue"), 
       pch=1)

# TODO:
# 1. Compare more
