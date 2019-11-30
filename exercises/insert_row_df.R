# Insert a row in between

insert.df <- function(df, v, idx, v.name=NA){
  df.new <- rbind(df[1:idx-1, ], v, df[idx:nrow(df), ])
  
  # DOUBT: rbind bug? When the row names are 1:nrow(df)
  #        The row after the inserted row has the name (idx+1)(1)
  #        For e.g. idx=3, the next row has name 31
  # TODO: Efficient way to check if two vectors are equal?
  if (all(row.names(df) == 1:nrow(df))){
    row.names(df.new) <- NULL
  }
  if (!is.na(v.name)){
    row.names(df.new)[idx] <- v.name
  }
  return(df.new)
}

df <- data.frame(A=1:5, B=6:10, C=11:15)
rownames(df) <- c("a", "b", "c", "d", "e")

insert.df(df, 16:18, 3, "f")
