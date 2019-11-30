# addmargins to a table

voters <- data.frame(
  vote.for.x=sample(c("yes", "no", "not sure"), 1e3, replace = T),
  voted.for.x.last.time=sample(c("yes", "no"), 1e3, replace = T),
  age=as.integer(rnorm(1e3, 35, 5))
)

# Yes, you can pass a dataframe
t <- table(subset(voters, select = -age))
addmargins(t)

my.addmargins <- function(t){
  t <- cbind(t, apply(t, 1, sum))
  t <- rbind(t, apply(t, 2, sum))
  
  colnames(t)[ncol(t)] <- "sum"
  rownames(t)[nrow(t)] <- "sum"
  return(t)
}
my.addmargins(t)
