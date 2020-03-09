# Dummy variables

df <- read.csv("data/EXAM_REGRESSION.csv")
# Correctly specified model as in practical exercise
df <- df[, -c(1, 6)]
n <- nrow(df)
df[, "DUM.M"] <- c(rep(1, 14), rep(0, n-14))
df[, "DUM.F"] <- c(rep(0, 14), rep(1, n-14))

make.X <- function(X){
  as.matrix(cbind(1, X))
} 

# y = b1 + b2X2 + b3X3 + b4X4 + b5DUM.F + b6DUM.M
lm.mod1 <- lm(Y~., data=df)
summary(lm.mod1)

# The above summary shows NA value for the DUM.M estimate
# This is due to perfect multicollinearity
# We can verify this as the inverse of (X'X) inverse does NOT exist
X <- make.X(df[-1, ])

# TODO: Add a try catch
A <- solve(t(X)%*%X)

# Let's drop DUM.F and run lm
lm.mod2 <- lm(Y~., data=df[, -which(colnames(df) == "DUM.F")])
summary(lm.mod2)

# You get the same value for the coefficient
# Let's drop DUM.F and run lm
lm.mod2 <- lm(Y~., data=df[, -which(colnames(df) == "DUM.M")])
summary(lm.mod2)

# The b5 coefficient is significantly different from 0
# Male and female are not equal at alpha = 0.1 level of significance
# Our estimate of the difference if b5
# women are earning b1 + b5, men are earning b1

# Suppose X2 is the experience column.
# Question: Female experienced student earning more than
#           Male experienced student
# 1st way: Only experience matters and interacts with gender
# y <- b2X2 +b3X3 + b4X4 + b5DUM.M +b6X2
# Refer: lm without intercept (t.ly/AbxRK)

# without intercept
lm.mod3 <- lm(Y~X2+X3+X4+DUM.M+DUM.M*X2 - 1, data=df)
summary(lm.mod3)

# TODO: Create X matrix

# 2nd way: Gender separately and also experience matters and 
#          interacts with gender
# y <- b1 + b2X2 +b3X3 + b4X4 + b5DUM.M +b6X2
lm.mod4 <- lm(Y~X2+X3+X4+DUM.M+DUM.M*X2, data=df)
summary(lm.mod4)

# TODO: Create X matrix
