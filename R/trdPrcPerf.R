#' Trading Price Performance
#' 
#' Trading Price Performance or trading PnL is the measure of cost during trading and tell whether the investor 
#' did better or worse as compared to arrival price.
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
#' 
trdPrcPerf <- function(Portfolio, Symbol, side="Buy")
{
  pname <- Portfolio
  #If there is no table for the symbol then create a new one
  if(is.null(.getPortfolio(pname)$symbols[[Symbol]]))
    addPortfInstr(Portfolio=pname, symbols=Symbol)
  Portfolio <- .getPortfolio(pname)
  txns <- Portfolio$symbols[[Symbol]]$txn
  #print(txns$TxnQty)
  p_avg <- as.numeric(last(txns$Pos.Avg.Cost))
  p_0 <- as.numeric(txns$Pos.Avg.Cost[min(which(txns != 0))])
  
  if(side=="Buy") {
    side_sign=1
  }
  else {
    side_sign=-1
  }
  
  tpp <- (-1)*side_sign*((p_avg - p_0)/p_0)*10000
  print(p_0)
  print(p_avg)
  print(side_sign)
  print(tpp)
  res <- sum(tpp)
  return(res)
  
}
