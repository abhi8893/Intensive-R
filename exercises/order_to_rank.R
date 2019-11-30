# Convert order to rank
df <- data.frame(v1=c(6, 2, 5, NA, 2, 5, 2),
                 v2=c(673, 201, 523, 7, 199, 501, 200))

order(df$v1, df$v2)