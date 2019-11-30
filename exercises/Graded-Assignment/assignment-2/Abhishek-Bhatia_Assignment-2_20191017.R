#' ---
#' title: Assignment 2
#' author: Abhishek Bhatia
#' date: "17 October 2019"
#' ---
#' 
#' 
#' 1. Dice roll question <br />
#' i.) Two dice are rolled 1000 times. Find how many times the result from both
#'     the dice are divisible by 3.
#'    
# Consider 2 six sided dice from numbers 1 to 6.
die.vals <- 1:6
times <- 1e3
die1 <- sample(die.vals, times, replace = T)
die2 <- sample(die.vals, times, replace = T)

# How many times both were divisible by 3 ?
# Expected is (1/3)*(1/3)*1000 = 111.111
sum((die1 %% 3 == 0) & (die2 %% 3 == 0))

#' ii.) Repeat the process 1000 times and plot the histogram

nreps <- 1000
# Initialize a vector to store the result of the experiment
res <- vector("numeric", nreps)
for (i in 1:nreps){
  die1 <- sample(die.vals, times, replace = T)
  die2 <- sample(die.vals, times, replace = T)
  res[i] <- sum((die1 %% 3 == 0) & (die2 %% 3 == 0))
}

# Plot the histogram of the result
hist(res, main = "", prob = T, xlab="")
title("2 Dice roll, both divisible by 3", line=3)
# Looks like normal
curve(dnorm(x, mean=mean(res), sd=sd(res)), -80, 140, 1e3, add = T, col="red")
abline(v=mean(res), lty=2, col="green")
axis(3, at=mean(res), font=2, lwd.ticks = 3, col = "green")

legend("topleft",
       c("Fitted Normal distribution",
         "Mean"),
       col=c("red", "green"),
       lty=c(1, 2),
       bty="n")

#' 2. Write a function of your own that takes a vector x as input and evaluates
#'    the following two expressions together using loops.
#'    
calc.ab <- function(x, na.rm=F){
  
  # mean of x
  x.mu <- mean(x, na.rm = na.rm)
  n <- length(x)
  # Initialize both to 0
  a <- 0
  b <- 0
  for (i in 1:n){
    # If the element is NA and we want to exclude NA
    # skip the rest of the current iteration
    if (is.na(x[i]) & na.rm){
      next
    }
    a <- a + (x[i] - 2)
    b <- b + (x[i] - x.mu)^3
  }
  
  # Finally divide both a and b by n
  # Doing this to save computational time as I am dividing only once
  # and not evertytime in the loop
  a <- a/n
  b <- b/n
  
  return(c(a, b))
}

x <- c(1, 2, NA, 3, 5, 9, NA, 9, 10)
calc.res <- calc.ab(x, na.rm = T)
calc.res

# Check if the calculation is correct (as expected)
n <- length(x)
a <- sum(x - 2, na.rm = T)/n
b <- sum((x- mean(x, na.rm = T))^3, na.rm = T)/n
exp.res <- c(a, b)
all.equal(calc.res, exp.res)

#' 3. Four files are given of the names 
#'    A000.csv, A003.csv, A006.csv, A009.csv <br/>
#' i.) Load the files using loops and add the content of the files as rows
#'     in a dataframe

# Initialize an empty dataframe
d <- data.frame()
# The file data files are kept in data folder
data.dir <- "data"
nfiles <- 4

# load the files in a loop
for (i in 1:nfiles){
  fname <- paste0("A", "00", (i-1)*3, ".csv")
  f <- paste(c(data.dir, fname), collapse = "/")
  
  contents <- suppressWarnings(
    read.csv(f, header = F)
    )
  # Bind the contents to dataframe
  d <- rbind(d, contents)
  row.names(d)[i] <- fname
}
View(d)
d

#' ii.) Find the column wise mean and add the result as fifth row. Change the
#'      name of the fifth row to "Mean"
#'      
d <- rbind(d, colMeans(d))
row.names(d)[nfiles+1] <- "Mean"
View(d)
d
