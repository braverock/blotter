#' Arrival Cost
#' 
#' The Arrival Cost (or Trading Cost) is the difference between the
#' average execution price and the price of the asset at the time the
#' order was entered into the market, ie. the Arrival Price.
#' 
#' Positive means a cost, negative means a saving.
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
#' The Index Cost is the difference between the
#' average execution price and the price of the Index (or ETF) at the time the
#' order was entered into the market, ie. the Arrival Price.
#' 
#' To get the index VWAP of one does not have the underlying trades and weightings,
#' a commonly used proxy is an ETF tracking the relevant index.
#' 
#' Positive means a cost, negative means a saving.
#'
#' @param Portfolio A portfolio name that points to a portfolio object structured with initPortf()
#' @param IndexSymbol An instrument identifier for an index used for adjusting a stock's Arrival Cost (typicall ETF)
#'
#' @return
#' 
#' A numeric value in basis points
#' 
#' @export
#'
#' 
IndexCost <- function(Portfolio, IndexSymbol){
  pname <- Portfolio
  #If there is no table for the symbol then create a new one
  if(is.null(.getPortfolio(pname)$symbols[[IndexSymbol]]))
    addPortfInstr(Portfolio=pname, symbols=IndexSymbol)
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio$symbols[[IndexSymbol]]$txn
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  p_0 <- as.numeric(txns$Pos.Avg.Cost[min(which(txns != 0))])
  ic <- ((p_avg - p_0)/p_0)*10000
  ic
}

#' Index Adjusted Cost
#' 
#' A market or index adjusted metric accounting for price trend in a stock
#' due to the overall market.
#'
#' @param ArrivalCost value representing arrival cost
#' @param IndexCost value representing index cost
#' @param beta value representing stock K's sensitivity to the index
#'
#' @return
#' 
#' The Index Adjusted Cost
#' 
#' @export
#'
#' 
IndexAdjustedCost <- function(ArrivalCost, IndexCost, beta){
  iac <- ArrivalCost - (beta * IndexCost)
  cat(iac,"bps")
}