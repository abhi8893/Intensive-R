# Merge multiple lists

# BUG: Currently does not account for same key merging
merge.list <- function(...){
  lst <- list(...)
  lst <- lapply(lst, function (l) as.data.frame(l))
  
  as.list(Reduce(merge, lst))
}

# Three cases
# 1. Both are named
# 2. One is named, other is not (2 cases)
# 3. Both are half named

get.n.max <- function(l){
  max(unlist(lapply(l, function (v) length(v))))
}

lst.padval <- function(l, n, val=NA){
  lapply(l, function (v) c(v, rep(val, 
                                  abs(length(v) - n))))
}
merge.list <- function(...){
  lst <- list(...)
  n.max <- max(unlist(lapply(lst, function (l) get.n.max(l))))
  
  lst <- lapply(lst, function (l) lst.padval(l, n.max))
  lst <- lapply(lst, function (l) as.data.frame(l))
  Reduce(merge, lst)
}

