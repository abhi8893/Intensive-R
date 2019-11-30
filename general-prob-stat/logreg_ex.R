# Logreg example
aba <- read.csv("../data/artofr_data/Abalone.data", header = F)
# Read in the header names
# Warning message: Since the last line in a file should be empty
clmns <- readLines("../data/artofr_data/Abalone.names")
clmns <- strsplit(clmns, split = ", ")[[1]]
colnames(aba) <- clmns

# exclude infants
abamf <- aba[aba$Gender != "I", ]

# Logreg for predicting gender
# Use logreg individually for every column
lftn <- function(clmn){
  glm(abamf$Gender ~ clmn, family=binomial)$coef
}

lgreg.all <- sapply(abamf[, -1], lftn)

# class of glm result is "glm" "lm"
# This is because "glm" is a subclass of "lm"
class(glm(Gender ~ Length, data = abamf, family = binomial))

# Now suppose we want to do linreg for Length ~ Diameter
# Based on gender
# TODO: Extract models in a list with gender information?
# Function by is an object-oriented wrapper for tapply applied to data frames.
by(aba, aba$Gender, function(d) lm(d[, "Length"] ~ d[, "Diameter"]))
