# Show only dominant frequencies (k highest)

# TODO: Make the function NOT cavalier towards ties of freq
tabdom <- function(tbl, k, k.type = c("highest", "lowest")){
  # NOTE: The Freq column is automatically added
  d <- as.data.frame(tbl)
  
  k.type <- match.arg(k.type)
  if (k.type == "highest"){
    decreasing = T
  } else{
    decreasing = F
  }
  
  return(d[order(d$Freq, decreasing = decreasing), ][1:k, ])
  
  
}

v <- sample(1:5, 1e3, prob = c(0.7, 0.2, 0.05, 0.03, 0.02), replace = T)
tbl <- table(v)

tabdom(tbl, 2, "lowest")
