# Vectorized ifelse
# ifelse(test, yes, no)
# Ref: Pg42 Art of R Programming

v <- 1:10
ifelse(v %% 2 == 0, "even", "odd")

# yes and no can be vectors
v <- c(5, 2, 9, 12)
# square all numbers greater than 6
# cube otherwise
ifelse(v > 6, x**2, x**3)


# formal argument names
args(ifelse)

# Nested ifelse
v <- 1:10
ifelse(v == 1, "1", ifelse(v == 2, "2", "not_1_or_2"))

# NOTE: The return value of yes and no is recycled to the same length
#       as v, and yes[i] is returned if TRUE, else no[i]



ab <- read.csv("../data/artofr_data/Abalone.data", header = F, as.is = T)
colnames(ab)[1] <- "gender"
ab["gender.val"] <- ifelse(ab[, 1] == "M", 
                           1,
                           ifelse(ab[, 1] == "F", 2, 3))
