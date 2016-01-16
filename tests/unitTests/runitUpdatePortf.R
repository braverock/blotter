# Author: Peter Carl, RUnit port by Ben McCann

test.txnFees <- function() {
  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"))
    try(rm_stocks(symbols))
    try(rm(list=c(p1,a1), pos=.blotter))
    try(rm(IBM))
  })

  currency("USD")
  symbols <- c("IBM")
  for (symbol in symbols){
    stock(symbol, currency="USD", multiplier=1)
  }
  data(IBM, package="blotter")

  ## simple portfolio with one transaction
  p1 <- initPortf(name="p1runitUpdatePortf", symbols=symbols)
  p1 <- addTxn(Portfolio="p1runitUpdatePortf", Symbol="IBM", TxnDate='2007-01-04', TxnQty=100, TxnPrice=96.5, TxnFees=-0.05*100)
  p1 <- updatePortf(Portfolio="p1runitUpdatePortf", Dates='2007-01-03::2007-01-10')
  a1 <- initAcct(name="a1runitUpdatePortf", portfolios="p1runitUpdatePortf")
  a1 <- updateAcct(a1,'2007-01')
  a1 <- updateEndEq(a1,'2007-01')

  ## (really) simple transaction cost function
  fiveCents <- function(qty, prc, ...) return(-0.05*qty)
  p2 <- initPortf(name="p2runitUpdatePortf", symbols=symbols)
  p2 <- addTxn(Portfolio="p2runitUpdatePortf", Symbol="IBM", TxnDate='2007-01-04', TxnQty=100, TxnPrice=96.5, TxnFees=fiveCents)
  p2 <- updatePortf(Portfolio="p2runitUpdatePortf", Dates='2007-01-03::2007-01-10')
  a2 <- initAcct(name="a2runitUpdatePortf", portfolios="p2runitUpdatePortf")
  a2 <- updateAcct(a2,'2007-01')
  a2 <- updateEndEq(a2,'2007-01')

  checkEquals(getAccount(a1)$summary$End.Eq, getAccount(a2)$summary$End.Eq)
}
