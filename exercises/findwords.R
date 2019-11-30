# Make a list of indices of a word in a text file

findwords1 <- function(f, sortby=c(NA, 'word', 'freq'), decreasing = F){
  txt <- scan(f, "")
  l <- list()
  for (i in seq_along(txt)){
    l[[txt[i]]] <- c(l[[txt[i]]], i)
  }
  
  
  sortby <- match.arg(sortby)
  if (!is.na(sortby)){
    if (sortby == 'word'){
      l <- l[sort(names(l), decreasing = decreasing)]
      } else if (sortby == 'freq'){
      freqs <- sapply(l, length)
      l <- l[order(freqs, decreasing = decreasing)]
      }
    }
  return(l)
}

findwords1("../data/artofr_data/testconcorda.txt", 
          sortby = 'freq', decreasing = T)


# New york times article on R
# word freq plot of top 10 %
# wordfreq ordered by freq
l <- findwords1("../data/artofr_data/nyt.txt", sortby = "freq", decreasing = T)
nwords <- length(l)
freqs.top10 <- sapply(l[1:round(0.1*nwords)], length)
# Plot!
barplot(freqs.top10, las=2)



# findwords can be simplified by using split
findwords2 <- function(f){
  txt <- scan(f, "")
  words <- split(seq_along(txt), txt)
  return(words)
}

findwords2("../data/artofr_data/testconcorda.txt")

f <- "../data/artofr_data/testconcorda.txt"

# TODO: Suppress stdout
# TODO: Do it with a larger file
microbenchmark::microbenchmark(
  findwords1(f),
  findwords2(f)
)
