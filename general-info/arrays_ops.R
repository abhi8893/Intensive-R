# arrays in R

firsttest <- rbind(c(46, 30), c(21, 25), c(50, 50))
secondtest <- rbind(c(46, 43), c(41, 35), c(50, 50))

tests <- array(data=c(firsttest, secondtest), dim=c(3, 2, 2))
class(tests)
attributes(tests)
dim(tests)

# firsttest
tests[,,1]
# secondtest
tests[,,2]

# Another way to stack matrices
# Ref: https://t.ly/KJwzP
