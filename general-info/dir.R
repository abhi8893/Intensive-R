# dir function to get directory structure
# Ref: Pg 245 Art of R Programming
# Sum of Contents of many files


is.numericfile <- function(f){
  is.file <- !file.info(f)$isdir
  is.numfile <- tools::file_ext(f) == "numeric"
  
  return(is.file & is.numfile)
}

sumtree <- function(dir.loc, files.include){
  for (f in fls){
    tot <- 0
    if (files.include(f)){
      # Or use read.csv with header = F
      v <- suppressWarnings(
        as.numeric(strsplit(readLines(f), split = ", ")[[1]])
      )
      tot <- tot + sum(v)
    }
  }
  
  return(tot)
}

dir.loc <- "../data/artofr_data/to_sum_files/"

sumtree(dir.loc, is.numericfile)
