# Count factor values

count.unique <- function(v, prop=T){
  v.tab <- table(v)
  
  if (prop){
    prop.table(v.tab)
  } else{
    v.tab
  }
}

resp <- c("yes", "no", "may be")
probs <- c(0.7, 0.2, 0.1)
names(prob) <- resp

# Data collected
feedback <- sample(resp, 1000, replace = T, prob = probs)

# Plot!
barplot(count.unique(feedback))


# yes <- 5, no <- 10, maybe <- 10

feedback <- c(rep("yes", 5), rep("no", 10), rep("maybe", 10))

# count
feedback.counts <- table(feedback)

# Plot!
barplot(feedback.counts)
