# Difference between NA and NULL
# NA - Unknown
# NULL - non-existent
# Ref: Pg 43 Art of R Programming

v1 <- c(NA, 1, 2)
v1
mode(v1[1]) # NA takes the mode of the vector 
mode(v1[2])

mode(NA) # logical

v2 <- c(NULL, 1, 2)
v2
mode(v2[1])
mode(v2[2])

z <- NA
for (i in 1:10) if (i %% 2 == 0) z <- c(z, i)
z

z <- NULL
for (i in 1:10) if (i %% 2 == 0) z <- c(z, i)
z

mode(NA) 
mode(NA_character_)
mode(NA_integer_)
mode(NA_real_)
mode(NA_complex_)

mode(NULL)
