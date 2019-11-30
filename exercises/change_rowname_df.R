# Change the row name for particular rows in a data frame

change.rownames <- function(names, idx, df){
  row.names(df)[idx] <- names
  return(df)
}
