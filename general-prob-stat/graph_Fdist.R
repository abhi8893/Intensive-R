# graph F distribution

l <- list(c(2, 4), c(4, 6), c(12, 12))

graph.F <- function(df1, df2){
  curve(df(x, df1, df2), 0, 4, 1000, add = TRUE)
}

mapply(graph.F, df1=c(2, 4, 12), df2=c(4, 6, 12))

