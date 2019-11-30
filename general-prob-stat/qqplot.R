#
d.lst <- list(
  urban=c(3, 7, 25, 10, 15, 6, 12, 25, 15, 15, 7),
  rural=c(48, 44, 40, 38, 33, 21, 20, 12, 1, 18)
)

par(mfrow=c(1, 2))
for (grp in names(d.lst)){
  qqnorm(d.lst[[grp]], main = grp)
  qqline(d.lst[[grp]], distribution = qnorm, col='red')
}



par(mfrow=c(1, 1))
boxplot(d.lst$urban, d.lst$rural, names = c("urban", "rural"))

plot.qnorm.bplot <- function(v1, v2, names=NULL){
  d.lst <- list(v1=v1, v2=v2)
  if (!is.null(names)){
    names(d.lst) <- names
  }
  
  par(mfrow=c(1, 2))
  for (grp in names(d.lst)){
    qqnorm(d.lst[[grp]], main = grp)
    qqline(d.lst[[grp]], distribution = qnorm, col='red')
    abline()
  }
  
  par(mfrow=c(1, 1))
  boxplot(v1, v2, names=names(d.lst))
  
}

d.lst <- list(
  stimulus.1=c(91.5, 94.18, 92.18, 95.39, 91.79, 89.07, 94.72, 87.21),
  stimulus.2=c(89.19, 90.95, 90.46, 93.21, 97.19, 97.09, 91.07, 92.75))
plot.qnorm.bplot(d.lst$stimulus.1, d.lst$stimulus.2, names(d.lst))
