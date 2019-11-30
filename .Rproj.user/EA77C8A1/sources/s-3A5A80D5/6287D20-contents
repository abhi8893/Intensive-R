# General linear regression
df <- read.table("../data/artofr_data/ExamsQuiz.txt")
lma <- lm(FINAL ~ MIDTERM, data = df)
attributes(lma)
str(lma)
summary(lma) # summary.lm() is called behind the scenes
lma$coeff # partial matching

lmb <- lm(FINAL ~ MIDTERM + AVG_QUIZ, data = df)
summary(lmb)

# Coefficients
lmb$coeff
coef(lmb)
# Fitted
fitted(lmb)
# Residuals
residuals(lmb)
# Formula
formula(lmb)

# Plot!
par(mfrow=c(1, 2))

for (var in c("MIDTERM", "AVG_QUIZ")){
  lm.fit <- lm(df[[]])
  plot(df$MIDTERM, df$FINAL)
  abline(coef(lmb)[c("(Intercept)", var)])
}


par(mfrow=c(1, 1))
# Polynomial Regression
x <- sample(-10:10, 100, T)
y <- 7 - 5*(x^2) + rnorm(length(x), sd=10^2)

plot(x, y)
poly.fit <- lm(y ~ x + I(x^2))

lines(spline(x, fitted(poly.fit)), col="red")
