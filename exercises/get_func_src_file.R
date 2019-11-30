# Function to get source, print it to a file and open in Rstudio

# TODO: use getAnywhere
get.source <- function(func, fdir="/tmp/"){
  func.name <- substitute(func)
  fpath <- paste0(fdir, func.name, ".R")
  dput(func, file = fpath)
  system(paste("rstudio", fpath))
}
