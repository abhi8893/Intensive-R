# Implement aggregate function

df <- mtcars

l <- lapply(split(df, list(df$cyl, df$gear)),
       colMeans, na.rm = T)

res <- do.call(rbind, l)       
res <- res[!apply(res, 1, function (r) all(is.na(r))), ]

strsplit(row.names(res), ".")

my.aggregate <- function(df, by, FUN, ...){
  do.call(r)
}

