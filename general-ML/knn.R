
# Load dataset
df <- iris
head(df)

set.seed(123)

# train and test
test_size <- 0.4
n <- nrow(df)
rownum <- sample(1:n, size = round(test_size*n))
train <- df[-rownum, 1:4]
test <- df[rownum, 1:4]
actual <- df[rownum, 5]
cl <- as.factor(df[-rownum, 5]) # train labels


# train_test_split

accuracy <- function(y.true, y.pred){
  return(mean(y.true == y.pred))
}

preds <- knn(train, test, cl, k=2)

# Compute accuracy
accuracy(actual, preds)


# Do this for k = 1 to 15
max.k <- 15
scores.test <- vector("numeric", max.k)
scores.train <- vector("numeric", max.k)

for (k in 1:max.k){
  preds.test <- knn(train, test, cl, k)
  preds.train <- knn(train, train, cl, k)
  
  scores.test[k] <- accuracy(actual, preds.test)
  scores.train[k] <- accuracy(cl, preds.train)
}

# Plot!
plot(1:max.k, scores.test, type="l", ylim = c(0.8, 1), 
     col="red", xlab="K", ylab="Accuracy", main="Accuracy scores for KNN")
lines(1:max.k, scores.train, col="green")

axis(1, at=1:max.k)
legend(2, 0.9,
      c("test", "train"),
      col=c("red", "green"), lty=1)
