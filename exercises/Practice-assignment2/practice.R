
# Q1 
df <- airquality
res <- aggregate(subset(df, select=-Day), by=list(df$Month), 
                 FUN = mean, na.rm=T)2
# Drop month column
res <- res[, -ncol(res)]

res[, "Group.1"] <- sapply(res[, "Group.1"], FUN = function (m) month.name[m])

# Change column names
colnames(res) <- c("Month", paste0("Avg.", colnames(res)[2:ncol(res)]))


# Q2
df <- mtcars

# i. Give the names of the cars in order of decreasing mpg
rownames(df[order(df$mpg, decreasing = T), ])

# ii. find how many different types of cyl is there for cars having
#     mpg greater than 18

length(unique(subset(df, mpg > 18, select = cyl)$cyl))


# iii. make a bar plot to show how many cars are there with
#      different cyl above mpg 18

# iv. rank the cars (only names) in decreasing order of wt only till
#     wt = 3
df.cyl.gt18 <- subset(df, mpg > 18, select=c(cyl))

cyl.gt18 <- df.cyl.gt18$cyl
names(cyl.gt18) <- rownames(df.cyl.gt18)

# Ref: Stackoverflow: all names in x-axis barplot https://t.ly/dxdWw
barplot(cyl.gt18, names.arg = names(cyl.gt18), las = 2)

# iv. rank the cars (only names) in decreasing order of wt only till
#     wt = 3

row.names(df[order(df$wt, decreasing = T), ])


# Q3
df <- PlantGrowth
df$group <- as.factor(df$group)
cols <- c("red", "green", "blue")[as.factor(df$group)]
plot(df$weight, df$group, col = cols, ylab = "Type", xlab = "")


# Q4
# i. Load practice.txt in R into a data frame with two columns as
#    A and B from the file.

# Check first 5 lines to find a pattern
# They seem to be : separated
f <- "Practice-assignment2/practice.txt"
for (l in readLines(f, n = 5)){
  cat(l, "\n")
}

df <- read.table(f, sep = ":")

# ii. Compute the mean row wise and add as column
res <- cbind(df, rowMeans(df))
colnames(res)[ncol(res)] <- "rowmean"

# iii. Compute the mean column wise and add as row
res <- rbind(res, colMeans(res))
rownames(res)[nrow(res)] <- "colmean"

# iv. Save the modified data frame as csv file
ofile <- "Practice-assignment2/practice_output.txt"
write.csv(res, ofile)
