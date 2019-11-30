# Swap the columns of a dataframe
swap.cols <- function(df, swap){
  swap.names <- colnames(df)[swap]
  df[swap] <- df[rev(swap)]
  colnames(df)[swap] <- rev(swap.names)
  df
}


swap.cols(df, c(1, 3))