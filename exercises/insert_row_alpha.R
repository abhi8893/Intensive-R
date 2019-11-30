# Insert a new row into a data frame based on the true alphabetical order

insert.df <- function(df, l, byrow=T, bycol=F, method="alphabetically"){
  byrow <- byrow*T
  
  if (byrow){
    df <- rbind(df, v)
    df[order(row.names(df)), ]
  } else{
    df <- cbind(df, v)
    df[, order(colnames(df))]
  }
}
