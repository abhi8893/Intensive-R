#' ---
#' title: Exercises (Statistical Methods)
#' author: Abhishek Bhatia
#' date: "13th September 2019"
#' ---
#' 1. The following data represents the daily high temperature 
#'    (in degree Celcius) on July 4 in New Delhi over a sequence of 30 years:
v <- c(22.8, 26.2, 31.7, 31.1, 26.9, 28.0, 29.4, 28.8, 26.7, 27.4, 28.2, 30.3, 
       29.5, 28.9, 28.9, 27.5, 28.3, 24.1, 25.3, 28.5, 27.7, 24.4, 29.2, 30.3,
       33.7, 27.5, 29.3, 30.2, 28.5, 32.2, 33.7)
#' (a) Present the data in a frequency histogram
# Sturges formula
num.bins <- round(1 + 3.322*log10(length(v)))
num.bins

par(mfrow=c(2, 2))
# Ref: Specifying smaller bins than Sturges formula https://t.ly/XNE5O
l <- lapply(4:7, function (b) hist(v, breaks = seq(min(v), max(v), length.out=b), 
                                   main = paste("Bins:", b)))
# Reset mfrow
par(mfrow=c(1, 1))

hist(v, probability = T, main = "Daily high Temp (degC) on July 4, New Delhi")
curve(dnorm(x, mean=mean(v), sd=sd(x)), 22, 34, add=T, col = "red")
abline(v=summary(v)[c("Mean", "Median")], col=c("blue", "green"))
legend("topleft", 
       legend = c("Mean", "Median"), col=c("blue", "green"), 
       lty=1)


# Since the median and mean lie very close to each other and also the data
# seems to come from a normal distribution, which visually seems to fit quite
# well, we can say the data is more or less symmetric

#' 2. A set of 200 observations was broken into 8 classes each of size 3, and 
#' the frequency of values in each class was determined. A frequency table was
#' then constructed. But some of the values are missing from the frequency 
#' table.
# Number of observations
N <- 200
class.interval <- rep(NA, length = 8)
class.interval[4] <- "15-18"
freq <- c(NA, 14, 18, 38, NA, 42, 11, NA)
rel.freq <- c(0.05, NA, NA, NA, 0.1, NA, NA, NA)
#' This is how the initial dataframe looks like:
df <- data.frame(class.interval, freq, rel.freq)
df

#' Fill missing class intervals

get.class.width <- function(cls.int){
  cls.lims <- as.numeric(strsplit(cls.int, sep)[[1]])
  cls.width <- cls.lims[2] - cls.lims[1]
  return(cls.width)
}

get.class.interval <- function(cls.index, known.int="15-18", 
                               known.index=4, sep="-"){
  known.lims <- as.numeric(strsplit(known.int, sep)[[1]])
  cls.width <- known.lims[2] - known.lims[1]
  cls.lims <- known.lims + (cls.index - known.index)*cls.width
  cls.int <- paste(cls.lims, collapse = "-")
  return(cls.int)
  
}

class.interval <- sapply(seq_along(class.interval), get.class.interval)
class.interval

#' Fill missing frequency
# WARNING: Possibly redundant calculations

# find where rel.freq is not NA
rel.freq.notNA <- !is.na(rel.freq)
freq[rel.freq.notNA] <- rel.freq[rel.freq.notNA]*N

freq
# Last one is missing 
if (sum(is.na(freq)) == 1){
  freq[is.na(freq)] <- N - sum(freq, na.rm = T)
} else{
    print("Error: Cannot fill, more than 1 value missing")
  }

#' Fill missing relative frequency
# WARNING: Possibly redundant calculations

rel.freq[!rel.freq.notNA] <- freq[!rel.freq.notNA]/N

df <- data.frame(class.interval, freq, rel.freq)

# TODO: Assign automatic colors from a color object class (cmap)
barplot(df$rel.freq, names.arg = df$class.interval, space = 0,
        ylim = c(0, 0.25))

#' 3. Consider the following table: <br />
#' Let's create the desired dataframe as mentioned in the assignment:
# Find number of class intervals
min.cls.int <- "15-20"
secndlst.cls.int <- "70-75"

min.lims <- as.numeric(strsplit(min.cls.int, "-")[[1]])
cls.width <- min.lims[2] - min.lims[1]
secndlst.lims <- as.numeric(strsplit(secndlst.cls.int, "-")[[1]])

n.cls.int <- ((secndlst.lims[2] - min.lims[1])/cls.width) + 1
n.cls.int

#' Make the Age of drivers(years) column
age.of.drvrs <- sapply(seq(n.cls.int), 
                         get.class.interval,
                         known.int = min.cls.int, 
                         known.index = 1)

age.of.drvrs[n.cls.int] <- "Over 75"
age.of.drvrs

#' Make the % of all drivers column
# perc drivers
perc.drvrs <- c(9, 13, 13, 11, 9, 8, 8, 7, 6, 6, 4, 3, 3)
perc.drvrs

#' Make the % of all drivers in fatal accidents column
# perc drivers in fatal accident
perc.drvrs.fatal <- c(18, 21, 14, 11, 7, 6, 5, 5, 4, 3, 2, 2, 2)
perc.drvrs.fatal

#' This is how our dataframe looks like:
df <- data.frame(age.of.drvrs, perc.drvrs, perc.drvrs.fatal)
df

#' (a) Draw a relative frequency histogram for age breakdown of drivers.
barplot(df$perc.drvrs, names.arg = df$age.of.drvrs, las = 2, space=0, 
        xlab = "", ylab = "% of all drivers")
# Move xlab farther away to avoid clutter
# Ref: https://t.ly/bz3vA
title(xlab="Age of drivers (years)", line=4, cex.lab=1)


#' (b) Draw a relative frequency for the age breakdown of those drivers who are
#' killed in car accidents.
barplot(df$perc.drvrs.fatal, names.arg = df$age.of.drvrs, las = 2, space=0,
        xlab = "", ylab = "% of all drivers in fatal accidents")
title(xlab="Age of drivers (years)", line=4, cex.lab=1)

#' (c) Which age group accounts for the largest number of fatal accidents? <br/>
#' From histogram it's 20-25 age group
as.character(
  subset(df, perc.drvrs.fatal == max(perc.drvrs.fatal))$age.of.drvrs
)

#' (d) Which age group should be charged the highest premium?. Explain your
#' reasoning. <br />
#' Insurance premium will be charged the highest for the age group
#' where the probability of accidents is highest within that group
#' Also other factors to take into account: Age i.e. older the person
#' more is the chance of him/her sustaining serious injuries (If the insurance
#' refers to health insurance)
#' This would be the the age group with the highest ratio of % of all drivers
#' in fatal accidents/ % of all drivers

rat <- df$perc.drvrs.fatal/df$perc.drvrs
as.character(subset(df, rat == max(rat))$age.of.drvrs)
