# Extract nth element of a dataframe

nth.df <- function(n, df, byrow=T, bycol=F){
  r <- nrow(df)
  
  if (bycol){
    r <- nrow(df)
    i <- n %% r
    j <- n - i + 1
  } else {
    c <- ncol(df)
    j <- n %% c
    j <- n - i + 1
  }
  
  return(df[i, j])
  
  
}

nth.df(2, df)
