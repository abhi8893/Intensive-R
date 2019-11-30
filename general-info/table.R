# table function

voters <- data.frame(
  vote.for.x=sample(c("yes", "no", "not sure"), 1e3, replace = T),
  voted.for.x.last.time=sample(c("yes", "no"), 1e3, replace = T),
  age=as.integer(rnorm(1e3, 35, 5))
)

# Yes, you can pass a dataframe
t <- table(subset(voters, select = -age))
addmargins(t)

# find mean age per group
res <- aggregate(voters$age, by=subset(voters, select=-age), mean)
res
# TODO: Pivot res into a table

v <- sample(c("yes", "no", "maybe"), 1e3, replace = T)
table(v)


gender <- sample(c("M", "F"), 1e3, replace = T)
education <- sample(c("UG", "PG"), 1e3, replace = T)
religion <- sample(c("H", "M", "C", "B"), 1e3, replace = T)
df <- data.frame(gender, education, religion)

table(df)
