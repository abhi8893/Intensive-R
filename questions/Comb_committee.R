# Ref: Pg 205 Art of R Programming
# Three committees of size 3, 4, and 5, are chosen from 20 people. What is the
# probability that persons A and B are chosen for the same committee?


sim <- function(nreps){
  commdata <- list()
  commdata$countabsamecomm <- 0
  
  for (rep in 1:nreps){
    commdata$whosleft <- 1:20
    commdata$numabchosen <- 0
    
    # Any way to implement nested for loop
    # with next referencing to outer for loop?
    commdata <- choosecom(commdata, 5)
    if (commdata$numabchosen > 0) next
    
    commdata <- choosecom(commdata, 4)
    if (commdata$numabchosen > 0) next
    
    commdata <- choosecom(commdata, 3)
    if (commdata$numabchosen > 0) next
    
  }
  
  print(commdata$countabsamecomm/nreps)
}


choosecom <- function(comdat, comsize){
  committee <- sample(comdat$whosleft, comsize)
  comdat$numabchosen <- length(intersect(1:2, committee))
  
  if (comdat$numabchosen == 2){
    comdat$countabsamecomm <- comdat$countabsamecomm + 1
  }
  
  comdat$whosleft <- setdiff(comdat$whosleft, committee)
  return(comdat)
}


sim(1000)
