# Implement set operations

find.unique <- function(v){
  elems <- c()
  for (e in v){
    if (! e %in% elems){
      elems <- c(elems, e)
    }
  }
  
  return(elems)
}

has.elem <- function(elem, v){
  status <- F
  for (e in v){
    if (e == elem){
      status <- T
      break
    }
  }
  return(status)
}

set.union <- function(A, B){
  A.uniq <- unique(A)
  B.uniq <- unique(B)
  
  return(unique(c(A.uniq, B.uniq)))
}

set.intersect <- function(A, B){
  int.set <- rep(0, length(A))
  
  i <- 0
  for (a in A){
    for (b in B){
      if ((a == b) & (!a %in% int.set)){
        i <- i + 1
        int.set[i] <- b
      }
    }
  }
  
  int.set <- int.set[1:i]
  return(int.set)
}

set.diff <- function(A, B){
  diff.set <- rep(0, length(A))
  
  n.B <- length(B)
  i <- 0
  for (a in A){
    i.B <- 1
    for (b in B){
      if (a == b){
        break
      } else if(i.B == n.B){
        i <- i + 1
        diff.set[i] <- a
      }
      i.B <- i.B + 1
    }
  }
  
  diff.set <- diff.set[1:i]
  
  return(diff.set)
}

set.diff(c(1, 2, 3, 4, 99), c(5, 3, 2, 1, 12, 1, 87))

set.equal <- function(A, B){
  n.A <- length(A)
  n.B <- length(B)
  if (n.A >= n.B){
    longer <- A
    shorter <- B
  } else {
    longer <- B
    shorter <- A
  }
  
  n.s <- length(shorter)
  for (l in longer){
    i.s <- 1
    for (s in shorter){
      if (l == s){
        break
      }else if (i.s == n.s){
        return(FALSE)
      }
      i.s <- i.s + 1
    }
  }
  
  return(TRUE)
}

set.equal(c(1, 2), c(1, 2, 1, 1))
