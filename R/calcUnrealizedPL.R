calcUnrealizedPL <- function(TradingPL, RealizedPL) {
  UnrealizedPL <- TradingPL - RealizedPL
  return(UnrealizedPL)
}
