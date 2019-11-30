#' ---
#' title: Assignment (Probability theory)
#' author: Abhishek Bhatia
#' date: "2nd November 2019"
#' ---
#' Define some global variables
max.nrep <- 1e3 # maximum number of trials to perform
coin.sides <- c(1, 0) # H <- 1, T <- 0

die.faces <- 1:6
card.nums <- sapply(1:13, function (x) ifelse(x < 10, paste0("0", x), x))
card.deck <- lapply(c("H", "D", "S", "C"), 
                  function (nm) paste0(nm, card.nums))
card.deck <- Reduce(c, card.deck)

choices.lst <- list(
  coins=coin.sides,
  dice=die.faces,
  cards=card.deck
)

#' Function to initialize an empty dataframe with specified datatypes 
#' for columns. This will help us to store the occurence of event for increasing
#' number of trials.
init.emptydf <- function(nrow, ncol, data.types, 
                         col.names=1:ncol, row.names=NULL){
  d <- as.data.frame(
    lapply(data.types, function (dtype) vector(dtype, nrow))
  )
  colnames(d) <- col.names
  rownames(d) <- row.names
  return(d)
}
init.emptydf(10, 5, rep("logical", 5))

#' Function to extract an attribute from a list of lists
#' nl - nested list
extract.attr <- function(attr.nm, nl){
  lapply(nl, function (l) l[[attr.nm]])
}


#' A general function to perform an experiment  <br/>
#'  - consisting of `nrep` trials <br/>
#'     - number of trials to perform: "numeric" <br/>
#'  - in each trial `num.iters` are performed <br/>
#'     - number of tosses of a coin / draw of cards in each trial: "numeric" <br/>
#'  - event identifier functions event.idfuncs to identify the occurence of an  <br/>
#'    event/ multiple events in a trial <br/>
#'     - a list of objects of class "idfunc" : list # TODO: (*Not Fully Implemented) <br/>
#'  - `type` of experiment to peform <br/>
#'     - whether a coin toss experiment/ card draw experiment <br/>
#' - biasedness of a coin with head probability head.prob <br/>
#'     - the probability of getting a head: "numeric" # TODO: (*To Be Implemented) <br/>
#'       <DEFAULT: 0.5> <br/>


perform.exp <- function(num.iters, nrep, event.idfuncs, 
                        type, replace = T, choices=NULL){
  
  # Appropriate sample space for one iter of each trial
  if (is.null(choices)){
    choices <- choices.lst[[type]]
  }
  
  # Extract datatypes and cast into a vector
  # Do I really need different datatypes or just logical for event occurence?
  dtypes <- Reduce(c, extract.attr("rtrn.type", event.idfuncs))
  # Create an empty df to store the event occurences in each trial
  event.df <- init.emptydf(nrep, 
                           length(event.idfuncs),
                           dtypes, col.names = names(event.idfuncs))
  
  # Now perform the experiment with nrep trials
  for (i in 1:nrep){
    # In each trial
    # Toss the coin num.coins times
    res <- sample(choices, num.iters, replace = replace)
    # Store the occurence of the events in the row
    # BUG: Assigning a list to a row doesn't work as I expect it to.
    suppressWarnings(event.df[i, ] <- 
      lapply(event.idfuncs, function (idfunc) idfunc$f(res))
    )
    
  }
  # Frequency definition of probability.
  p <- colMeans(event.df)
  return(p)
}


#' Function to identify if a vector has atleast two consecutive vals or not.
has.consec <- function(res, val){
  last.val <- res[1]
  for (i in 2:length(res)){
    cur.val <- res[i]
    if (last.val == val & cur.val == val){
      return(TRUE)
    }
    last.val <- cur.val
  }
  return(FALSE)
}

#' Event identifier functions
event.idfuncs <- list()

#' Coin draw experiment
idfuncs.coins <- list(
  is.head=function(res) res == 1,
  first.is.head=function(res) ifelse(res[1] == 1, T, F),
  atleast.one.head=function(res) any(res == 1),
  none.is.head=function(res) !any(res != 1),
  all.are.head=function(res) all(res == 1),
  atleast.2.consec.head=function(res) has.consec(res, 1)
)

# Define the return type values for each function
event.idfuncs$coins <- lapply(idfuncs.coins, 
                              function(idf) list(f=idf, rtrn.type="logical"))


#' Die roll experiment
idfuncs.dice <-  list(
  sum.gt.10=function(res) sum(res) >= 10
)

event.idfuncs$dice <- lapply(idfuncs.dice, 
                              function(idf) list(f=idf, rtrn.type="logical"))

#' Card draw experiment
make.card <- function(card.str){
  card <- list(suit=substr(card.str, 1, 1), 
               number=substr(card.str, 2, 3))
  card$color <- ifelse(card$suit %in% c("D", "H"), 'red', 'black')
  
  class(card) <- 'card'
  return(card)
}

idfuncs.cards <- list(
  is.ace=function (card) make.card(card)$number == '01',
  is.black=function(card) make.card(card)$color == 'black',
  is.diamond=function(card) make.card(card)$suit == 'D',
  is.ace.or.diamond=function (card) c(make.card(card)$number == '01' | 
                                        make.card(card)$suit == 'D'))

event.idfuncs$cards <- lapply(idfuncs.cards, 
                              function(idf) list(f=idf, rtrn.type="logical"))


#' This is what we are going to get the estimate of probability after 1000 trails
#' of tossing 4 identical coins.
perform.exp(4, 1e3, event.idfuncs$coins, type='coins')

#' Function to perform the experiment with different number of trials
#' Returns a simulation dataframe.
#' TODO: Implement using cumulative mean instead of repeating the experiment
#'       This way it will be much much less compute intensive. 
make.sim.df <- function(exp.func, max.nrep, ...){
  # Simulate and find the estimate of probability 
  # by the frequency definition, by carrying out various experiments
  # with increasing number of trials.
  sim.df <- Reduce(rbind.data.frame, 
                   lapply(1:max.nrep, 
                          function (nrep) exp.func(..., nrep=nrep)
                   )
                   )
  colnames(sim.df) <- names(list(...)$event.idfuncs)
  return(sim.df)
}

#' Function to plot the simulation dataframe with the axiomatic probability. 
plot.sim.df <- function(sim.df, axm.prob, mfrow){
  par(mfrow=mfrow)
  max.nrep <- nrow(sim.df)
  for (event in colnames(sim.df)){
    
    p <- axm.prob[[event]]
    plot(1:max.nrep, sim.df[, event], type='l', 
         xlab = "Number of trials (n)", ylab = "m(E)/n", yaxt='n',
         main = event)
    axis(2, at=c(0, p, 1), 
         labels = c("0", 
                    as.character(MASS::fractions(p)), 
                    "1"))
    abline(h=axm.prob[[event]], col='red')
  }
}

#' 1. Probability of head after tossing a fair coin.
# Calculated axiomatic probability
axm.prob <- list(is.head=1/2) 
# Probability estimate using the frequency definition
sim.df <- make.sim.df(perform.exp, max.nrep, type='coins', num.iters=1, 
                      event.idfuncs=event.idfuncs$coins['is.head'])
# Plot it!
plot.sim.df(sim.df, axm.prob, c(1, 1))

#' 2. Probability of getting atleast one head in two tosses of a fair coin.
axm.prob <- list(atleast.one.head=3/4)
sim.df <- make.sim.df(perform.exp, max.nrep, type='coins', num.iters=2, 
                      event.idfuncs=event.idfuncs$coins['atleast.one.head'])
plot.sim.df(sim.df, axm.prob, c(1, 1))

#' 3. Probability of getting no head in three tosses of a fair coin.
axm.prob <- list(none.is.head=1/8)
sim.df <- make.sim.df(perform.exp, max.nrep, type='coins', num.iters=3, 
                      event.idfuncs=event.idfuncs$coins['none.is.head'])
plot.sim.df(sim.df, axm.prob, c(1, 1))
#' 5. A fair coin is tossed 4 times: <br/>
#' (a) First flip is head. <br/> 
#' (b) Atleast one flip is head. <br/>
#' (c) All the flips are head. <br/>
#' (d) Atleast 2 consecutive heads. <br/>

# We do not need is.head event identifier function in case of 4 tosses
efuncs <- event.idfuncs$coins[-which(names(event.idfuncs$coins) %in% c("is.head"))]
axm.prob <- list(
  first.is.head=1/2,
  atleast.one.head=15/16,
  none.is.head=1/16,
  all.are.head=1/16,
  atleast.2.consec.head=1/2
)
sim.df <- make.sim.df(perform.exp, max.nrep, 
                      type='coins', num.iters=4, 
                      event.idfuncs=efuncs)

plot.sim.df(sim.df, axm.prob, c(3, 2))


#' 4. Sum of the faces >= 10
axm.prob <- list(
  sum.gt.10=1/6
)
sim.df <- make.sim.df(perform.exp, max.nrep, type='dice', 
                      num.iters=2, event.idfuncs=event.idfuncs$dice)

plot.sim.df(sim.df, axm.prob, c(1, 1))

#' 6. Pick a card from a standard deck of 52

axm.prob <- list(
  is.ace=4/52,
  is.black=26/52,
  is.diamond=13/52,
  is.ace.or.diamond=16/52
)
sim.df <- make.sim.df(perform.exp, max.nrep, 
                      type='cards', num.iters=1, 
                      event.idfuncs=event.idfuncs$cards)

plot.sim.df(sim.df, axm.prob, c(2, 2))

#' 7. An integer is chosen at random from {1, 2, .... 200}
#' Probability that integer is divisible by 2 or 3.

idfunc <- list(is.divby.2or3=list(
  f=function(res) res %% 2 == 0 | res %% 3 == 0,
  rtrn.type='logical'))
axm.prob <- list(
  is.divby.2or3=133/200
)

sim.df <- make.sim.df(perform.exp, max.nrep, num.iters=1, 
                      event.idfuncs=idfunc, choices=1:200)

plot.sim.df(sim.df, axm.prob, c(1, 1))
