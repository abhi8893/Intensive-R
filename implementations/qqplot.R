# qqplot for comparing theoritical quantiles and calculated corresponding
# quantiles from observed data
# Ref: PSU STAT 414 <- https://t.ly/ePkXE



make.qqplot <- function(v, probs, distr.fun){
  th.q <- distr.fun(probs)
  v.order <- sort(v)
  
  # v.q <- sapply(probs, function(p) )
  
}