#' ---
#' title: Midsem
#' author: Abhishek Bhatia
#' date: "25 October 2019"
#' ---
#' NOTE: Click on Compile report (Ctrl+Shift+K) icon to get an HTML 
#'       report of this script. <br/>
#' 1. <br/>
#' (a) Create a dataframe with given column and row names as
d <- data.frame(list(A=1:4, 
                     B=c(3, 2, 7, 8),
                     C=c(9, 5, 4, 1)),
                row.names=paste0("D", 1:4)
                )
d
View(d)

#' (b) Perform (A*B)/C row wise and add the result as the fourth column 
#'     in the dataframe with the column name as "Output" without using loops.

# Apply a function to each row and column bind it to the dataframe

apply.and.bind <- function(X, MARGIN, FUN, bind.name){
  if (MARGIN == 1){
    bind.func <- cbind
  } else {
    bind.func <- rbind
  }
  X <- bind.func(X, 
             setNames(
               list(apply(X, MARGIN, FUN)),
             bind.name)
             )
  
  return(X)
}

d <- apply.and.bind(d, 1, 
                    function(v) v["A"]*v["B"]/v["C"], 
                    "Output")
d
View(d)

#' (c) Add another column with the column name as "Digitized Output" whose
#'     value is decided based on the criterion (without using loops) as <br/>
#'      -> Output >= 1, Digitized Output = 1 <br/>
#'      -> Output < 1, Digitized Output = 0 <br/>
d <- apply.and.bind(d, 1, 
                    function(v) ifelse(v["Output"] >=1, 1, 0), 
                    "Digitized Output")
d
View(d)

#' (d) Save the final dataframe with four rows and five columns as Result.txt in
#'     your working directory with row names and column names

write.table(d, "Result.txt")

#' 2. Create a 5x5 matrix as shown below using loops where the values of each
#'    element is a product of the row index and the column index

mat <- matrix(nrow=5, ncol=5)
mat
View(mat)

for (i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    mat[i, j] <- i*j
  }
}

mat
View(mat)

#' 3. 
#' (a) Read the four files FN01.csv, FN05.csv, FN09.csv and FN13.csv using loops.
#'     The file names should be generated inside loops.
#' (b) Create a dataframe and insert the content of the files as rows.
#' (c) The row names of the dataframe should be FN01, FN05, FN09, FN13

# Define data directory
data.dir <- "data"

# Define a function to get the ith file name
get.fname <- function(i){
  ith.term <- 1 + (i-1)*4
  if (ith.term <= 9){
    ith.term <- paste0("0", ith.term)
  }
  fname <- paste0("FN", ith.term)
  return(fname)
}

nfiles <- 4
d <- data.frame()
# Read the files in a loop, and rbind them to the dataframe
for (i in 1:nfiles){
  fname <- get.fname(i)
  f <- paste(c(data.dir, paste0(fname, ".csv")), collapse="/")
  contents <- read.table(f, header = T)[, 1]
  d <- rbind(d, contents)
  rownames(d)[i] <- fname
}
colnames(d) <- 1:5
d
View(d)

#' (d) Save the dataframe as "Combined.csv" file with the rownames.
write.csv(d, "Combined.csv")


#' 4. Using the OrchardSprays dataset
# Verify it's a data.frame
d <- OrchardSprays
is.data.frame(d)
#' (a) Find the total number of decrease for treatment A. Code should return
#'     a number.

# total number of decrease
sum(d$decrease)

#' (b) Which treatment is having the maximum decrease.
#'     Code should return the treatment type only.
# Sort by decreasing order of decrease, 
# select treatment column, first element 
# for treatment with maximum decrease
as.vector(d[which.max(d$decrease), "treatment"])

#' (c) Create a dataframe which should have all the columns as the dataset but
#'     the rows are ordered in descending order of decrease values.

d2 <- d[order(d$decrease, decreasing = T), ]
head(d2)
View(d2)
# If so desired the row names can be reset
rownames(d2) <- NULL
head(d2)
View(d2)
