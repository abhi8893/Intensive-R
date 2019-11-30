#' ---
#' title: Assignment Re-test (Intro to R)
#' author: Abhishek Bhatia
#' date: "26th September 2019"
#' ---
#' 
#' 1. Use the “AirPassengers” dataset for the following
#' 
# It's a time series object
class(AirPassengers)

ts.obj <- AirPassengers
# Extract year for each observation
year <- floor(time(AirPassengers))
year

# Since it's a time series object it has a cycle
# We need to extract the sampling times at each observation i.e. Months
month <- month.abb[cycle(ts.obj)]
month

# Finally we can create the dataframe
# Now our data is in tidy format (one row for each observation)
df <- data.frame(AirPassengers=as.matrix(ts.obj), year=year, month=month)
head(df)

#' i) Create dataframe with month name for the dataset as below:
df2 <- df[, -which(colnames(df) == "year")]
head(df2)
#' ii) Find the monthly average passengers for each months
df.monmean <- 
  aggregate(df[, -which(colnames(df) %in% c("year", "month"))],
            by=list(df$month), mean, na.rm = T)

colnames(df.monmean) <- c("month", "Mean.AirPassengers")
# The months are ordered alphabetically
df.monmean

# Convert month column into ordered factor vector
# with order corresponding to the actual month timing
# Create a function to sort a df by month
sort.monthly <- function(df, month.col){
  df[, month.col] <- factor(df[, month.col], levels = month.abb, ordered = T)
  df <- df[order(df[, month.col]), ]
  return(df)
  
}
# Present the results in month time-sorted order
sort.monthly(df.monmean, "month")

#' iii) Plot the monthly variance for each month
df.monvar <- aggregate(df$AirPassengers, by=list(df$month), var, na.rm = T)
colnames(df.monvar) <- c("month", "Variance.AirPassengers")
# Present the results in month time-sorted order
df.monvar <- sort.monthly(df.monvar, "month")

plot(df.monvar$Variance.AirPassengers, pch=1, xaxt="n", xlab="Month",
     ylab="variance", main="Monthly variance of AirPassengers")

axis(1, at=1:12, labels=df.monvar$month)

#' 2. Create a vector having the following elements <br />
#' 2, 3, -4, 5, 9, 1, -3, 2
v <- c(2, 3, -4, 5, 9, 1, -3, 2)
#' Compute log 2 of all the elements of vec1
log2.v <- log2(v)
log2.v

log2.v.nan.ind <- is.nan(log2.v)
#' Plot the result in y-axis and element values in the x-axis as point plot. 
#' The NaN produced should be replaced with 0 and appear as red in the plot.
cols <- rep("black", times = length(v))
# red color where log2 of the vector is NaN
cols[log2.v.nan.ind] <- "red"
# Finally replace NaN with 0
log2.v[log2.v.nan.ind] <- 0
# plot!
plot(v, log2.v, col = cols, 
     main="Plot of log2(x)", 
     xlab="x",
     ylab="log2(x)")

# Add tickmarks corresponding to values in the vector
axis(1, at = v[!log2.v.nan.ind], lend=1, font=2, lwd.ticks = 3, col = "black")
axis(1, at = v[log2.v.nan.ind], lend=1, font=2, lwd.ticks = 3, col = "red")

legend("topleft",
       c("NaN values", "computed values"),
       col=c("red", "black"),
       pch = 1
)

#' Import the dataset datafile.csv and find the following
f <- "data/datafile.csv"
# Check what kind of file we have by reading in first 3 lines
for (l in readLines(f, 3)){
  cat(l, "\n")
}
# The file seems to be comma separated, so we can use sep = "," or read.csv
df <- read.csv(f)
head(df)

#' i. How many entries are present where husband’s age is less than wife’s age?
sum(df$age_husband < df$age_wife)

#' Compute the total income as addition of husband’s income and wife’s income to
#' find the households having total income more than 500000. Code should return
#' corresponding household number.
row.names(
  subset(df, (income_husband + income_wife) > 500000)
)


#' Create an additional column with the criterion as 
#' “Status” having values as <br />
#' Both husband and wife age more than 65: “Level 1” <br />
#' Husband greater than 65 but wife less than 65: “Level 2” <br />
#' Both Less than 65 but greater than equal to 55: “Level 3” <br />
#' Both less than 50: “Level 4” <br />
status <- rep(NA_character_, length=nrow(df))
cond.lst <- list()

cond.lst[["hus.wf.gt65"]] <- which((df$age_husband > 65 &(df$age_wife > 65)))
cond.lst[["hus.gt65.wf.lt65"]] <- which((df$age_husband > 65 & (df$age_wife < 65)))
cond.lst[["hus.wf.lt65.ge55"]] <- which(
  (df$age_husband < 65 & df$age_husband >= 55)
  &
    (df$age_wife < 65 & df$age_wife >= 55)
  )
cond.lst[["hus.wf.lt50"]] <- which(
  (df$age_husband < 50)
  &
    (df$age_wife < 50)
)

# Now we have the indices of the rows corresponding to the conditions
# stored in a list
str(cond.lst)

# Store the status names (levels) according to the condition
status.levels <- list(
  hus.wf.gt65="Level 1",
  hus.gt65.wf.lt65="Level 2",
  hus.wf.lt65.ge55="Level 3",
  hus.wf.lt50="Level 4"
)

# Assign values to status vector
for (n in names(status.levels)){
  ind <- cond.lst[[n]]
  level.name <- status.levels[[n]]
  status[ind] <- level.name
}

df[, "Status"] <- status
head(df)

#' Also find how many household are there in each of the levels 
#' such that to create an external file (.csv)

status.df <- data.frame(t(table(df$Status)))
status.df
# Remove Var1 column
status.df <- status.df[, -which(colnames(status.df) == "Var1")]
colnames(status.df) <- c("Status", "Number of Households")
status.df

# Write the file 
ofile <- "output/Household_status.csv"
write.csv(status.df, ofile, row.names = F)
