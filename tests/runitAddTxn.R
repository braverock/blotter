# Author: Peter Carl, RUnit port by Ben McCann

#Sys.setenv(TZ="America/Chicago")        # as the data set got save with this TZ
#options("width"=78)                     # to tie down the print() statement width
#verbose <- FALSE

test.addTxn <- function() {
  currency("USD")
  symbols <- c("IBM")
  for (symbol in symbols){
    stock(symbol, currency="USD", multiplier=1)
  }
  data(IBM)                               # data included in package

  # Initialize a portfolio object 'p'
  # Creating portfolio:
  p = initPortf(symbols=symbols)

  # Trades must be made in date order.
  # Make a couple of trades in IBM
  addTxn(p, "IBM", '2007-01-03',  50,  96.5,  TxnFees=0.05 * 50)
  addTxn(p, "IBM", '2007-01-04', -50,  97.1,  TxnFees=0.05 * 50)
  addTxn(p, "IBM", '2007-01-08', -10,  99.2,  TxnFees=0.05 * 10)
  addTxn(p, "IBM", '2007-01-09', -10, 100.1,  TxnFees=0.05 * 10)
  addTxn(p, "IBM", '2007-01-17', -10, 100.25, TxnFees=0.05 * 10)
  addTxn(p, "IBM", '2007-01-19',  30,  95,    TxnFees=0.05 * 30)
  addTxn(p, "IBM", '2007-01-22',  25,  96.3,  TxnFees=0.05 * 25)
  addTxn(p, "IBM", '2007-01-23',  25,  96.42, TxnFees=0.05 * 25)
  addTxn(p, "IBM", '2007-01-26', -25,  97.52, TxnFees=0.05 * 25)
  addTxn(p, "IBM", '2007-01-31', -25,  98.80, TxnFees=0.05 * 25)

  portfolio <- getPortfolio(p)
  transactions <- portfolio[["IBM"]][["txn"]]
  checkEquals(13, sum(transactions$Txn.Fees))
  checkEquals(0, sum(transactions$Txn.Qty))

  # TODO: fix bug in calcPortfSummary
  # summary <- calcPortfSummary(portfolio)
}

.tearDown <- function() {
  rm(list=ls(all=TRUE))
  .blotter <- new.env()
  .instrument <- new.env()
}
