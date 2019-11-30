# Get class of all columns of a dataframe

df <- data.frame(a=1:5, b=letters[1:5], c=vector(mode="logical", length=5),)

sapply(df, class)

df <- data.frame(a=1:5, b=letters[1:5], c=vector(mode="logical", length=5),
                 stringsAsFactors = F)
sapply(df, class)
