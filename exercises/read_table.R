# Read a file from a url and save it to a file
url <- "https://raw.githubusercontent.com/mhurd/R/master/ExamsQuiz.txt"
fname <- strsplit(url, "/")[[1]]
fname <- fname[length(fname)]
df <- read.table(url, header = T)
write.table(df, paste0("../data/artofr_data/", fname))

