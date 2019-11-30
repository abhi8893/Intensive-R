#' ---
#' title: Laundry Bill
#' date: "29th September 2019"
#' --- 
#' Problem statement: How much do each have to pay? <br />

#' Total Weights of clothes of each
weights <- list(
  Prax=.875,
  AB=1.855,
  AK=3.130,
  SH=2.22
)
weights

#' Price as listed on the bill
price <- list(
  WF=list(wt=6.5, amt=325, qty=19),
  WI=list(wt=1.5, amt=120, qty=5)
)
price



#' Let's verify that weights recorded by us and that mentioned
#' in the bill is approximately the same. <br />
#' They seem more or less equal.
# Weight recorded by us
cat('Weight recorded by us: ', 
    Reduce(sum, weights))

# Weight mentioned in the bill
# TODO: Modify the function sum somehow and pass original list
cat('Weight mentioned in the bill', 
    Reduce(sum, lapply(price, function (l) l$wt)))


#' Only AB and AK have Wash and Iron (WI)
WI.perunit.amt <- price$WI$amt/price$WI$qty
WI.perunit.wt <- price$WI$wt/price$WI$qty
WI.ppl <- list(AB=list(), AK=list())

WI.ppl$AB$amt <- 2*WI.perunit.amt
WI.ppl$AK$amt <- 3*WI.perunit.amt

WI.ppl$AB$wt <- 2*WI.perunit.wt
WI.ppl$AK$wt <- 3*WI.perunit.wt

WI.ppl


#' Modify weights for Wash and Fold calculation by subtracting 
#' the weights from the Wash and Iron 
weights.mod <- weights
weights.mod$AB <- weights.mod$AB - WI.ppl$AB$wt
weights.mod$AK <- weights.mod$AK - WI.ppl$AK$wt
weights.mod

#' Now let's calculate WF price
# Per kg amount for Wash and Fold
WF.perkg.amt <- price$WF$amt/price$WF$wt
# Multiply modified weights with per kg amount for wash and fold
price.WF <- lapply(weights.mod, function (x) x*WF.perkg.amt)

#' Let's calculate total price by adding the wash and Iron
#' price to Wash and Fold price
price.total <- price.WF
price.total$AB <- price.total$AB + WI.ppl$AB$amt
price.total$AK <- price.total$AK + WI.ppl$AK$amt

price.total

#' Add tax 
tax.pct <- 0.18
price.final <- lapply(price.total, function (x) x*(1 + tax.pct))
price.final


#' Verify it's approximately the same as mentioned in the bill
price.bill <- 525
cat('Price mentioned in the bill', price.bill)
cat('Price calculated', Reduce(sum, price.final))


