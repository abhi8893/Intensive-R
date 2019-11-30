# Sort one vector according to the value of the 2nd vector
# TODO: Implement using ranks

sort_v1_v2 <- function(v1, v2, descending = F){
  # Sort v1 based on v2
  v1[sort(order(v2, descending = descending)[v1])]
  
}