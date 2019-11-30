
choices.lst <- list(
  coin=coin.sides,
  cards=card.deck
)
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
    event.df[i, ] <- suppressWarnings(
      lapply(event.idfuncs, function (idfunc) idfunc$f(res))
    )
  }
  # Frequency definition of probability.
  p <- colMeans(event.df)
  return(p)
}



idfuncs.coin <- list(
  is.head=function(res) res == 1,
  first.is.head=function(res) ifelse(res[1] == 1, T, F),
  atleast.one.head=function(res) any(res == 1),
  none.is.head=function(res) !any(res != 1),
  all.are.head=function(res) all(res == 1),
  atleast.2.consec.head=function(res) has.consec(res, 1)
)

# Define the return type values for each function
event.idfuncs <- list()
event.idfuncs$coins <- lapply(idfuncs.coin, 
                        function(idf) list(f=idf, rtrn.type="logical"))


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

perform.exp(1, 1e3, event.idfuncs$cards,
            type = 'cards', replace = F)

make.sim.df(exp.func, )

