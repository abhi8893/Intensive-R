# read and write files

A <- data.frame(a=c(1, 2, 3), b=c(2, 4, 6))
write.table(A, file="../output/A.txt", row.names = F)
read.table("../output/A.txt", header = TRUE)

write.table(A, file="../output/A.txt", row.names = T)
read.table("../output/A.txt", header = TRUE)

# read a file as a matrix
matrix(scan("../data/artofr_data/x.txt"), nrow=5, byrow=T)

# general function
read.matrix <- function(filename){
  as.matrix(read.table(filename))
}


