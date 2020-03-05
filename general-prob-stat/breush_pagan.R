# Breush pagan test
library(lmtest)
fpath <- 'data/MLR_data.csv'
df <- read.csv(fpath)


lm.mod <- lm(MEDV ~ ., df)
