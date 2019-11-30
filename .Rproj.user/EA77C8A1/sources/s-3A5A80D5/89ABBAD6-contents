#' ---
#' title: Assignment (Intro to R)
#' author: Abhishek Bhatia
#' date: "19th September 2019"
#' ---
#' 
#' 1. The scores of four teams for three rounds in a competition is saved in 
#' three files as r1.txt, r2.txt and r3.txt
#' 
#' 
#' i) Import data from the three files and create a data frame with the team
#' name as row names and the scores of the rounds in three different
#' columns <br />
fnames <- paste0("r", 1:3, ".txt")
# Suppressing warning as the R expects the last line must be empty
# Reading in each file as a list of dataframes
contents <- suppressWarnings(lapply(fnames, 
                                    function(f) read.table(paste("data", 
                                                                 f,
                                                                 sep = "/")
                                                           )))
names(contents) <- fnames

# list of dataframes
contents

# Since the row names of dataframes are unordered, below code will give
# added rowname column to each dataframe in list
lapply(contents, function(d) data.frame(d, rowname = row.names(d)))

# Above will be helpful to merge multiple dataframes
# Finally we can apply Reduce by applying merge, 
# Reduce function: applies a function successively in a cumulative manner 
df <- Reduce(merge, lapply(contents, 
                           function(d) data.frame(d, rowname = row.names(d))))

# Add rownames
row.names(df) <- df[, "rowname"]

# Finally dropping the rownum column
df <- df[ , -which(names(df) == "rowname")]
df

#' ii) Find the total of the four teams and add as column in the data
df[, "Team.Total"] <- rowSums(df)
df

#' iii) Create a file which includes the winning team name with its total marks
#' as a list (Name of the teams should be taken from the files).
winning.team <- df[which.max(df$Team.Total), ]

# TODO: Write in a more DRY (Don't Repeat Yourself) way.
to.write <- paste0(
  "Team Name",
  "\n",
  row.names(winning.team),
  "\n",
  "Team Total",
  "\n",
  winning.team$Team.Total,
  "\n")

# This is what we want to write
cat(to.write)

# Write to the output file
writeLines(to.write, con = "output/winning_team.txt")

# Or capture the exact print output to the file
output <- list("Team Name"=row.names(winning.team), 
               "Team Total"=winning.team[, "Team.Total"])

# TODO: Regenerate the object using capture.output output str
capture.output(output, file = "output/winning_team2.txt")


#'2. Write a function of your own that takes in two 2x2 matrices (A and B) and <br /> 
#'   performs <br />
#' matrix multiplication (without using *%*). <br />
#' Output from that function should be a list with the following objects, <br />
#' i) A <br />
#' ii) B <br />
#' iii) AB <br />
#' iv) Whether AB = BA (Either TRUE or FALSE) <br />


matmulalgo <- function(A, B){
  AB <- matrix(0, 2, 2)
  # TODO: Implement using some apply function
  for (i in 1:2){
    for (j in 1:2){
      AB[i, j] <- sum(A[i, ]*B[, j])
    }
  }
  
  return(AB)
}


matmul <- function(A, B){
  # Check if shape is 2x2
  shape <- lapply(list(A, B), function(X) identical(dim(X), c(2L, 2L)))
  A.B.22 <- Reduce("&", shape)
  if (!A.B.22){
    stop("Unsupported dimensions: Please provide 2x2 matrices!")
  }
  
  AB <- matmulalgo(A, B)
  BA <- matmulalgo(B, A)
  
  AB.eq.BA <- identical(AB, BA)
  
  res <- list(A=A, B=B, AB=AB, AB.eq.BA=AB.eq.BA)
  return(res)
}


#' 3. Using state.x77 dataset, write line(s) of code to return the following <br />

# Converting to data frame since originally it's a matrix
df <- as.data.frame(state.x77)

#' i) Arrange the states in decreasing order of population. Among the first 10 
#' states which is having maximum Illiteracy (only name of the state) <br />
df.pop.dec <- df[order(df$Population, decreasing = T), ]
# Decreasing order of Population dataframe
head(df.pop.dec)

# Maximum illiteracy for df.pop.dec
row.names(subset(df.pop.dec[1:10, ], Illiteracy == max(Illiteracy)))


#' ii) Name the states where population is more than 15000 as well as income more
#' than 4500 <br />
subset(df, (Population > 15000) & (Income > 4500))

#' iii) Compute population per unit area (population/area) of all the states and add it
#' as a column with column name as “Population Density”. <br />

df[, "Population Density"] <- df$Population/df$Area

#' iv) Plot a bar graph with proper labels and title of top five states in Population
#' Density. <br />

df.popdens.dec <- df[order(df$`Population Density`, decreasing = T), ]


pop.dens.topN <- df.popdens.dec[1:5, "Population Density"]
names(pop.dens.topN) <- row.names(df.popdens.dec[1:5, ])

barplot(
  pop.dens.topN,
  cex.names = 0.9,
  ylab = "Population Density",
  xlab = "State",
  main = "Population Density of top 5 US States",
  col = rainbow(5)
)

