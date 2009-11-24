calcTradingPL <- function(PosValue,PrevPosValue,TxnValue) {
  TradingPL <- PosValue - PrevPosValue - TxnValue
  return(TradingPL)
}
