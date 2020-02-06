#' ---
#' title: ML Assignment 1
#' author: Abhishek Bhatia
#' date: "6th "
#' ---
library(cluster)
#' Load the dataset
df <- read.csv('data/data.csv')
# Drop the country column
X <- df[, -1]
#' Summary statistics <br/>
#' We can see the features are not in the same range. Need to perform feature
#' scaling
summary(X)

#' Apply Normalization (mean = 0, variance = 1)
df.scaled <- as.data.frame(scale(X))
rownames(df.scaled) <- df[, "Country"]

#' 1. Perform k-medoid algorithm for the given dataset for K=3. 
#'    Report the three medoids of the final clusters formed and the 
#'    corresponding country name.
res.pam <- pam(df.scaled, k=3, metric = "euclidean")
cat("The three medoids are:\n")
print(res.pam$medoids)

#' 2. Give the country names in each of the three clusters found above.
rownames(res.pam$medoids)

#' 3. Plot J vs K curve for the dataset for k=1 to 10 using K-medoid only.

distance <- function(x, y, dist="minkowski", p=2){
  return(sum((x-y)**p))**(1/p)
}

J <- function(X, mu, ids){
  # Calculates the cluster quality metric
  # Parameters
  # X: Data matrix
  # mu: Cluster centroid
  # ids: Cluster index for data points
  
  n <- dim(X)[1]
  res <- 0
  i <- 1
  while(i <= n){
    pt <- X[i, ]
    cntrd <- mu[ids[i], ]
    res = res + distance(pt, cntrd)**2
    i <-  i + 1
  }
  
  return(res/n)
}

sub.vec <- function(x, y){
  return(x[order(names(x))] - y[order(names(y))])
}


J.scores <- vector("numeric", 10L)
k.max <- 10

for (k in 1:k.max){
  res.pam <- pam(df.scaled, k=k, metric = "euclidean")
  mu <- res.pam$medoids
  ids <- res.pam$clustering
  ids <- ids[order(rownames(df.scaled))]
  J.scores[k] <- J(df.scaled, mu, ids)
}


#' The elbow can be seen at around K = 2
plot(J.scores, type='l', main = "J vs K plot", xlab = "K")

#' 4. Comment on the plot and the nature of the distribution of the dataset. <br/> 
#' There is a loose elbow at K = 2. But the elbow is not distinct, so the clusters
#' are very sparse and not compact.
#' 5. Perform hierarchical clustering on the dataset and report the dendogram.
res.hc <- hclust(dist(df.scaled), method="complete")
res.pam <- pam(df.scaled, 3)
plot(res.hc)

#' 6. Cut the tree such that you get three clusters and compare your results 
#'    with the k-medoids output with explanation. 
hc.clusters <- cutree(res.hc, 3)
pam.clusters <- res.pam$clustering

hc.clusters
pam.clusters


#' if the cluster id match
#' TRUE - where clusters match, FALSE - where clusters don't match
#' There is a good amount of match between the two methods
diff <- ifelse((hc.clusters - pam.clusters) == 0, TRUE, FALSE)
barplot(table(diff), main = "Comparison of hclust and pam")


