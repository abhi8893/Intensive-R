# Sorting and ordering of columns of a dataframe

df <-  USArrests
# Arrange based on UrbanPop decreasing order
m <- order(df$UrbanPop, decreasing = T)
df[m, ]


# Total_Crime
df['Total_Crime'] <- rowSums(subset(df, select=-c(UrbanPop)))
ordered.states <- row.names(df[order(df$Total_Crime, decreasing = T), ])

# Make a new dataframe based on ordered states
df.states <- data.frame(States=ordered.states)
