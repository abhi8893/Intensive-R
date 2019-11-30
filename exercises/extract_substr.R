# substring of a string

extract.substr <- function(s, start, stop){
  paste(strsplit(s, "")[[1]][start:stop],
        collapse = "")
}

s <- "a000ssnjksam s02n sjnajS"
extract.substr(s, 2, 8)

create.randomstr <- function(length, chars=letters){
  paste(sample(letters, length, replace = T),
        collapse = "")
}

s <- create.randomstr(1e4)

microbenchmark::microbenchmark(
  substr(s, 5432, 8906),
  extract.substr(s, 5432, 8906)
)
