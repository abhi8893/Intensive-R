#' ---
#' title: PCA Exercise (CISM)
#' author: Abhishek Bhatia
#' date: "14 Feb 2019"
#' ---

#' Load the data
df.raw <- read.table('data/bodyfat.txt', header = 1)
df <- scale(df.raw)

boxplot(df.raw)

#' Variance covariance matrix
# TODO: implement cov
cov.mat <- cov(df)

#' Eigen value decomposition
# TODO: implement eigen
pc <- eigen(cov.mat, TRUE)

#' Principal components
pc.vec <- pc$vectors

#' Compute the score vectors 

scores <- lapply(1:10, 
                 function (i) apply(df, 1, function(r) sum(r*pc.vec[, i])))       
names(scores) <- paste("PC", 1:10, sep="")
scores <- as.data.frame(scores)
head(scores)


#' Scree plot
var.exp <- (pc$values/sum(pc$values))
barplot(var.exp, names.arg = 1:10,
     xlab="PC", ylab="Explained variance", 
     main="Scree plot", type="l")


#' Cumulative scree plot
barplot(cumsum(var.exp), names.arg = 1:10,
        xlab="PC", ylab="Cumulative variance", 
        main="Scree plot", type="l")
abline(h=0.9, col="red")
axis(2, 0.9)
#' Project to 2-D space
prop.exp <- sum(var.exp[1:2])
plot(scores$PC1, scores$PC2)
text(-10, 0, 
     paste("proportion explained =", round(prop.exp, 2)),
     cex = 1 )

#' Score of a particular individual
v <- c(34, 90.8, 75, 89.2, 50, 34.8, 22, 24.8, 25.9, 16.9)
v.score <- apply(pc.vec, 2, function (clmn) sum(clmn*v))    
v.score

#' Data may or may not be scaled. Refer: t.ly/Lv36Y
#' PCA constructs orthogonal - mutually uncorrelated - linear combinations 
#' that (successively) explains as much common variation as possible. 
#' Actually, PCA can be done based on the covariance matrix as well as 
#' the correlation matrix, not only the latter one. Scaling the 
#' data matrix such that all variables have zero mean 
#' and unit variance (also known as "normalizing", "studentisizing", "z-scoring"), 
#' makes the two approaches identical. This is because the covariance 
#' between two normalized variables is the correlation coefficient.
#' 
#' 

#' How varied are the points in each of the PCs?
par(mfrow=c(2, 5))
for (i in 1:10){
  pc.name <- paste("PC", i, sep="")
  stripchart(scores[[pc.name]], xlim=c(-10, 10), pch = 0.1, main=pc.name)
}

