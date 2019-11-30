# Create a vector repeated using counts in another vector

# Method 1: Inefficient
resp <- c("yes", "no", "maybe")
counts <- c(5, 10, 10)
names(counts) <- resp

feedback <- c()

for (i in 1:length(resp)){
  feedback <- c(feedback, rep(resp[i], counts[i]))
}

# Method 2: One liner
feedback <- rep(resp, times = counts)
