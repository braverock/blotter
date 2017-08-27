#' Arrival Cost
#' 
#' The Arrival Cost (or Trading Cost) is the difference between the
#' average execution price and the price of the asset at the time the
#' order was entered into the market, ie. the Arrival Price.
#'
#'
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., "IBM"
#' @param side string identifying either "Buy" or "Sell"
#'
#' @return
#' 
#' A numeric value in basis points
#' 
#' @export
#'
#' @examples
#' 
ArrivalCost <- function(Portfolio, Symbol, side="Buy")
{
  pname <- Portfolio
  #If there is no table for the symbol then create a new one
  if(is.null(.getPortfolio(pname)$symbols[[Symbol]]))
    addPortfInstr(Portfolio=pname, symbols=Symbol)
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio$symbols[[Symbol]]$txn
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  p_0 <- as.numeric(txns$Pos.Avg.Cost[min(which(txns != 0))])
  
  ac <- if(side == "Buy"){
    1*((p_avg - p_0)/p_0)*10000
  } else if(side == "Sell"){
    -1*((p_avg - p_0)/p_0)*10000
  }
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
#' 
IndexCost <- function(IndexVWAP, IndexArrivalCost){
  ic <- ((IndexVWAP - IndexArrivalCost)/IndexArrivalCost)*10000
  ic
}

#'  Index Adjusted Cost
#'
#' @param ArrivalCost value representing arrival cost
#' @param IndexCost value representing index cost
#' @param beta value represent underlying stock beta to index
#'
#' @return
#' @export
#'
#' @examples
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