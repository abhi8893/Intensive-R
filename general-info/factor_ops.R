# Factor ops
v <- sample(letters[1:3], 10, replace=T)
v
vf <- as.factor(v)
class(vf)

# vf is just an instance of factor class (S3 class)
# so it's just a list with an added attribute of class
# and plus some others ("levels" in this case)
unclass(vf)


# Anticipate future new levels
vff <- factor(v, levels=letters[1:4])
vff

vff[2] <- "d"
vff

# Same in a factor vector with less levels would raise a warning
# and coerce the entry to NA
vf[2] <- "d"

# To overcome this we can append the levels of the factor vector
# to reflect the added entry
levels(vf) <- append(levels(vf), "d")
vf[2] <- "d"
vf

# more aptly using try-catch
v <- sample(letters[1:3], 10, replace=T)
v
vf <- as.factor(v)

# TODO: Do using tryCatch
v <- sample(letters[1:3], 10, replace=T)
v
vf <- as.factor(v)

# try to assign vf[2] <- "d", except a certain warning, do something