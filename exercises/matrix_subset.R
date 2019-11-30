# Subsetting in matrices 

m <-  matrix(1:9, 3, 3)
m[m >= 5]

# Fill NA or a value
subset(m, m[,]>=5)
