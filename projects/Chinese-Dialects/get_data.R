# Get the raw data from Matloff's website
chinese.raw <- readLines(paste("http://www.cs.ucdavis.edu/~matloff",
                               "matloff/public_html/145/Handouts/R2", 
                               "CanManB5.utf8", sep="/"))
save(file="chinese.raw.Rdata", chinese.raw )

# Create a tab-delimited version of the raw data
chinese <- sub(" ", "\t", chinese.raw)
chinese <- sub(" ", "\t", chinese)
chinese <- sub(" ", "\t", chinese)

# Function to split the data into fields and turn into a data frame
process.row <- function(string) {    
  temp <- unlist(strsplit(string, "\t"))
  return(data.frame(char=temp[1], Can=temp[2], Man=temp[3], Eng=temp[4], 
                    stringsAsFactors=FALSE))
}

# Create a data frame from the raw data
chinese.list <- lapply(chinese, process.row)
chinese.data <- do.call("rbind", chinese.list)
names(chinese.data)[1] <- "Ch char"

# Fix some cases with multiple Chinese pronunciations (I implicitly
# assume these are Cantonese pronunciations)
to.fix <- grep("^[[:alpha:]]+\\d", chinese.data$Eng, perl=TRUE)
chinese.data$Man[to.fix] <-
  gsub("^([[:alpha:]]+\\d) (.*)", "\\1", chinese.data$Eng[to.fix], perl=TRUE)
chinese.data$Eng[to.fix] <-
  gsub("^([[:alpha:]]+\\d) (.*)", "\\2", chinese.data$Eng[to.fix], perl=TRUE)

# Make two datasets: one for Cantonese, the other for Mandarin
can8 <- subset(chinese.data, select=c("Ch char", "Can"))
man8 <- subset(chinese.data, select=c("Ch char", "Man"))

# Ditch unneeded variables and save data to feed into the Chapter 5 example
rm(chinese.list, chinese, chinese.raw, chinese.data)
save(can8, man8, file="chinese.Rdata")