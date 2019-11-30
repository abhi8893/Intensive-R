# Read in the data

library(xlsx)
life <- read.xlsx("data/life.xlsx", sheetIndex = 1)

# summary stats
summary(life)

# Up until an Income of 1000, the life expectancy increases linearly with income
# After that it levels off, and doesn't increase as much faster
scatter.smooth(life$Income, life$Life)

# We can split the data upto Income level of 1000, and then apply regression
# separately.
life.fit <- lm(Life ~ Income, data = life)

# (b)
# (A large percentage) Most people have income less than 500
hist(life$Income)

# (c)
# Yes the plot suggests splitting the data as <1000 and >1000 would be a good 
# idea

# (d)
# split 
life.lt1000 <- subset(life, Income <= 1000)
life.gt1000 <- subset(life, Income > 1000)
plot(life.lt1000$Life, life.lt1000$Income)


# Q.2
body <- read.xlsx("data/body.xlsx", 1)

# (a) summary stats
summary(body)

# (b) Visually depict the distrn of fat
# Seems right skewed
hist(body$fat)

# (c) 
par(mfrow=c(1, 1))
body.agelt30 <- subset(body, age < 30)
body.agegt60 <- subset(body, age > 60)

# TODO: Add markers for mean, median, Q1, Q3
boxplot(body.agelt30$fat, body.agegt60$fat, main="Fat v/s age", 
        xlab = "fat", names = c("age < 30", "age > 60"), yaxt="n")
axis(2, at=c(median(body.agelt30$fat), median(body.agegt60$fat)),
             col = c("red", "green"))

# Summary stats for the two age classes
body.fatsmry <- cbind(summary(body.agelt30$fat), summary(body.agegt60$fat))
colnames(body.fatsmry) <- c("age.lt30", "age.gt60")
body.fatsmry


# CLT demonstration
# Take a sample of 200 observations 500 times
# The sample mean will tend towards normal distrbn 
# (sort of true, not rigourously appropriate to say this)
fat.sample <- matrix(sample(body$fat, (1e5), replace = T),
                     1e5/200, 200)

fat.smean <- colMeans(fat.sample)
hist(fat.smean, prob=T)
curve(dnorm(x, mean(fat.smean), sd(fat.smean)), 18, 22, 1000, add=T, col="red")

