#' Arrival Cost
#' 
#' The Arrival Cost (or Trading Cost) is the difference between the
#' average execution price and the price of the asset at the time the
#' order was entered into the market, ie. the Arrival Price.
#'
#' \deqn{ArrivalCost_bp} = Side \dot \frac{P_avg - P_0 }{P_0} \dot {10^4}_bp
#' \deqn{{p^s} = Pr( |r| > t-ratio)}{p^s = Pr(|r|>t-ratio)}
#'
#' @param side string identifying either "Buy" or "Sell"
#' @param p_avg value representing average executoin price
#' @param p_0 value representing arrival price
#'
#' @return
#' @export
#'
#' @examples
#' ArrivalCost(side = "Buy", p_avg = 100.50, p_0 = 100)
#' 
ArrivalCost <- function(side="Buy", p_avg=NULL, p_0=NULL){
  ac <- ifelse(side == "Buy",
               1*((p_avg - p_0)/p_0)*10000,
               -1*((p_avg - p_0)/p_0)*10000
  )
  ac
}

#' Index Cost
#'
#' @param IndexVWAP value representing the benchmark VWAP
#' @param IndexArrivalCost value representing the benchmark arrival price
#'
#' @return
#' @export
#'
#' @examples
#' IndexCost(IndexVWAP = 50100, IndexArrivalCost = 50000)
#' 
IndexCost <- function(IndexVWAP, IndexArrivalCost){
  ic <- ((IndexVWAP - IndexArrivalCost)/IndexArrivalCost)*10000
  ic
}

#'  Index Adjusted Cost
#'
#' @param ArrivalCost 
#' @param IndexCost 
#' @param beta 
#'
#' @return
#' @export
#'
#' @examples
#' ac <- ArrivalCost(side = "Buy", p_avg = 100.50, p_0 = 100)
#' ic <- IndexCost(IndexVWAP = 50100, IndexArrivalCost = 50000)
#' iac <- IndexAdjustedCost(ac, ic, 0.7)
#' 
IndexAdjustedCost <- function(ArrivalCost, IndexCost, beta){
  iac <- ArrivalCost - beta * IndexCost
  cat(iac,"bps")
}

# data <- read.csv("C:/Users/jasen/Personal/GitHub/31_trades_history")
# #txns <- xts(cbind(data$UNDERLYING_QUANTITY[7:48], data$UNDERLYING_PRICE[7:48]/100),as.POSIXct(data$TRADE_DATE[7:48]))
# #write.csv(txns, "C:/Users/jasen/Personal/GitHub/sample_txns.csv")
# txns <- read.csv("C:/Users/jasen/Personal/GitHub/blotter/demo/sample_txns.csv")
# 

