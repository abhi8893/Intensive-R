df <- USArrests

# Subset
## Method 1
subset(df, UrbanPop > 80)
## Method 2
df[df$UrbanPop > 80, ]


# UrbanPop > 80, select two columns
subset(df, UrbanPop > 80, select = c(Murder, Assault))

# Two conditions
subset(df, (UrbanPop > 80) & (Murder < 12))

# State with highest UrbanPop
rownames(subset(df, UrbanPop == max(UrbanPop, na.rm = T)))

# Find the names of the states where Murder > mean + 2*sd
rownames(subset(df, Murder > mean(Murder) +  2*sd(Murder)))

# Add three columns and make a new columns
df["Total_Crime"] <- rowSums(subset(df, select=c(-UrbanPop)))

# Find the state for which total crime is minimum
rownames(subset(df, Total_Crime == min(Total_Crime)))
         