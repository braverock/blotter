# Author: Peter Carl, RUnit port by Ben McCann

.tearDown <- function() {
  rm(list=ls(all=TRUE, pos=.blotter), pos=.blotter)
  rm(list=ls(all=TRUE, pos=.instrument), pos=.instrument)
}

test.txnFees <- function() {
  data(IBM)                               # data included in package
  currency("USD")
  symbols <- c("IBM")
  for (symbol in symbols){
    stock(symbol, currency="USD", multiplier=1)
  }

  ## simple portfolio with one transaction
  p1 <- initPortf(name="p1", symbols=symbols)
  p1 <- addTxn(Portfolio="p1", Symbol="IBM", TxnDate='2007-01-04', TxnQty=100, TxnPrice=96.5, TxnFees=0.05*100)
  p1 <- updatePortf(Portfolio="p1", Dates='2007-01-03::2007-01-10')
  a1 <- initAcct(name="a1", portfolios="p1")
  a1 <- updateAcct(a1,'2007-01')
  a1 <- updateEndEq(a1,'2007-01')

  ## (really) simple transaction cost function
  fiveCents <- function(qty, prc) return(0.05*qty)
  p2 <- initPortf(name="p2", symbols=symbols)
  p2 <- addTxn(Portfolio="p2", Symbol="IBM", TxnDate='2007-01-04', TxnQty=100, TxnPrice=96.5, TxnFees=fiveCents)
  p2 <- updatePortf(Portfolio="p2", Dates='2007-01-03::2007-01-10')
  a2 <- initAcct(name="a2", portfolios="p2")
  a2 <- updateAcct(a2,'2007-01')
  a2 <- updateEndEq(a2,'2007-01')

  checkEquals(getAccount('a1')$summary$End.Eq, getAccount('a2')$summary$End.Eq)
}
