# Convert Vector to Matrix without Recycling i.e padding with NA

vec.pad <- function(v, len, recycle = F, pad.val=NA){
  if (recycle){
    # Inefficient?
    v <- suppressWarnings(rbind(v, 1:len)[1, ])
  }else if (is.na(pad.val)){
    length(v) <- len
  } else{
    v <- c(v, rep(pad.val, len - length(v)))
  }
  
  return(v)
}
                      