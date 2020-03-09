# Breush pagan test
library(lmtest)
fpath <- 'data/EXAM_REGRESSION.csv'
df <- read.csv(fpath)[, -1]
X <- 

lm.mod <- lm(MEDV ~ ., df)

regrsrs <- c("X2", "X3", "X4", "X5")
Reduce(c, regrsrs)
regrsrs <- r