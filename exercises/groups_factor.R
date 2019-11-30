# Get groups from a factor vector

ab <- read.csv("../data/artofr_data/Abalone.data", header = F)
colnames(ab) <- c("Gender", "Length", "Diameter", "Height",
                  "WholeWt", "ShuckedWt", "ViscWt",
                  "ShellWt", "Rings")

# Or as.character(unique(ab[["gender"]]))
grp.names <- levels(ab[["Gender"]])
# NOTE: lapply will not give desired list here with names
grps <- sapply(grp.names, function(n) which(ab[["Gender"]] == n))


par(mfrow=c(2, 1))
# TODO: same xlim and ylim?
# Ref: https://t.ly/BnqrB
plot(ab[grps$M, "Length"], ab[grps$M, "Diameter"])
plot(ab[grps$F, "Length"], ab[grps$F, "Diameter"])

# TODO: Do above plotting using mapply


# new = FALSE Not working?
par(mfrow=c(1, 1))
plot(ab[grps$M, "Length"], ab[grps$M, "Diameter"])
plot(ab[grps$F, "Length"], ab[grps$F, "Diameter"], pch="x", new=FALSE)


# Compact
pchvec <- ifelse(ab$Gender == "M", "o", "x")
cols <- ifelse(ab$Gender == "M", "blue", "pink")
plot(ab$Length, ab$Diameter, pch=pchvec, col = cols)
