# data.frame is just a list whose elements are vectors of equal length
# Dataframe of unequal lengths
# TODO: A function with (ellipsis) ... args, and create a df with recycling or fill NA
# Refer: Stackoverflow <- choices and args https://t.ly/dWzY1
# Refer: Stackoverflow <- names of ... args https://t.ly/NEPDP

source("create_matrix_pad_NA.R")
source("get_args.R")
create.df <- function(..., recycle=F, extra.vals=NA){
  # TODO: Provide option for extra.vals to be a vector
  #       for a different padvalue for each column
  # TODO: Provide option for extra.vals to be a list
  #       for a different padvector for each column
  # TODO: Check if the argument is a list of vectors or a list
  #       and accordingly convert
  
  l <- get.arglist(...)
  max.len <- max(unlist(lapply(l, length)))
  if (recycle){
    l <- lapply(l, vec.pad, len = max.len, recycle = T) # Wrong!
  } else{
    l <- lapply(l, vec.pad, pad.val=extra.vals, len=max.len)
  }
  
  d <- data.frame(l)
    
  return(d)
}

v1 <- 1:3
v2 <- 1:4
v3 <- 1:2
create.df(v1, v2, v3)

l <- list(v1=v1, v2=v2, v3=v3)
# TODO: recycle = T is not being passed to the function
do.call(create.df, as.list(l, recycle=T))

