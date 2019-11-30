# Ref: Statistical methods slides

t <- c(0, 
       rep(1, 8),
       rep(2, 5),
       rep(3, 3),
       rep(4, 4),
       rep(5, 4),
       rep(6, 6),
       7,
       rep(8, 5),
       9,
       10,
       rep(11, 2),
       rep(12, 2),
       13,
       14,
       15,
       rep(16, 4),
       17,
       18,
       19)


status.idx <- list(doc=c(1, 41, 43, 51),
                   mtc=c(2:8, 10, 11:12, 15:16, 18, 32, 40, 42, 44, 47, 52),
                   alive=c(9, 13:14, 17, 19:31, 33:39, 45:46, 48:50, 53))

num.obs <- max(sapply(status.idx, max))

status <- vector(mode="character", length=num.obs)
subject <- seq(num.obs)

# TODO: Using sapply or lapply
for (n in names(status.idx)){
  status[status.idx[[n]]] <- n
}

df <- data.frame(subject, time=t, status)

my.mapvalues <- function(v, values, replace.with){
  lookup <- replace.with
  names(lookup) <- values
  
  func <- function(x){
    rep.val <- lookup[as.character(x)]
    names(rep.val) <- NULL
    if (is.na(rep.val)){
      return(x)
    } else{
      return(rep.val)
    }
  }
  return(sapply(v, func, USE.NAMES = F))
}


vital.status <- my.mapvalues(df[,"status"], 
                             values = c("mtc", "doc", "alive"),
                             replace.with = c(1, 0, 0))

my.mapvalues(vital.status, c(0, 1), )

split(vital.status, t)

