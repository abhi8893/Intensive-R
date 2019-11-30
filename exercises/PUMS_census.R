# Read PUMS Census file
# Ref: Pg 239 Art of R Programming

f <- "../data/artofr_data/PUMS5_06.TXT.short"

extract.pums <- function(pf, flds){
  d <- data.frame() # data frame to build
  con <- file(pf, "r") # establish connection with file
  
  repeat{
    hrec <- readLines(con, 1) # read household record
    if (length(hrec) == 0) break # EOF, leave loop
    
    # get household serial no.
    serno <- intextract(hrec, c(2, 8))
    
    # how many Person records?
    npr <- intextract(hrec, c(106, 107))
    
    if (npr > 0){
      for (i in 1:npr){
        prec <- readLines(con, 1)
        # make a row for this person to bind with dataframe
        person <- makerow(serno, prec, flds)
        # bind it with the dataframe
        d <- rbind(d, person)
      }
    }
  }
  
  return(d)
}

# make a list containing person's details
makerow <- function(srn, pr, fl){
  l <- list()
  l[["serno"]] <- srn
  for (nm in names(fl)){
    l[[nm]] <- intextract(pr, fl[[nm]])
  }
  return(l)
}

# extracts a substr as an integer
intextract <- function(s, rng){
  fld <- substr(s, rng[1], rng[2])
  return(as.integer(fld))
}



extract.pums(f, list(Gender=c(23, 23), Age=c(25, 26)))

