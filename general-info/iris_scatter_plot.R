# Plot a scatter plot of all the species in iris dataset with colors 
# Petal length v/s Petal width
# Setosa: red, Versicolor: green, Virginica: blue
# Ref: Plotting the Iris data https://t.ly/Ej35B
# Ref: R plot pch symbols https://t.ly/Bnq7E

scatter.plot <- function(df, x, y, group, cols, main, add.centroid=F){
  
  # Add a color column based on group
  df[,"colors"] <- sapply(df[[group]], function(sp) cols[[sp]])
  
  # Scatter plot by group 
  plot(df[[x]], df[[y]], col=df$colors, xlab=x, ylab=y, main=main)
  
  if (add.centroid){
    # Mean by group 
    df.grpmean <- aggregate(subset(df, select = c(x, y)), 
                            by = list(df$Species), mean)
    # Overlay centroid
    points(df.grpmean[[x]], df.grpmean[[y]], pch = 19)
  }
  
  legend("bottomright", 
         legend = names(cols), 
         col = unlist(cols),
         pch = 1, # Type of point symbol
         cex = 1, # Font size of legend
         bty = 'n') # legend box type
  
  
}

df <- iris
group <- "Species"
cols <- list(setosa="red", versicolor="green", virginica="blue")

scatter.plot(df=iris, x="Petal.Length", y="Petal.Width", 
             group="Species", 
             cols=list(setosa="red", 
                       versicolor="green", 
                       virginica="blue"),
             main="Iris Dataset",
             add.centroid = T)




