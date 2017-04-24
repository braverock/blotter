# Author: Peter Carl, RUnit port by Ben McCann

test.addTxn <- function() {
  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"))
    try(rm_stocks(symbols))
    try(rm(list=paste0("portfolio.",p), pos=.blotter))
  })

  currency("USD")
  symbols <- c("IBM")
  for (symbol in symbols){
    stock(symbol, currency="USD", multiplier=1)
  }
  data(IBM, package="blotter")

  # Initialize a portfolio object 'p'
  # Creating portfolio:
  p <- initPortf("runitAddTxn", symbols=symbols)

  # Trades must be made in date order.
  # Make a couple of trades in IBM
  addTxn(p, "IBM", '2007-01-03',  50,  96.5,  TxnFees=-0.05 * 50, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-04', -50,  97.1,  TxnFees=-0.05 * 50, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-08', -10,  99.2,  TxnFees=-0.05 * 10, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-09', -10, 100.1,  TxnFees=-0.05 * 10, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-17', -10, 100.25, TxnFees=-0.05 * 10, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-19',  30,  95,    TxnFees=-0.05 * 30, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-22',  25,  96.3,  TxnFees=-0.05 * 25, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-23',  25,  96.42, TxnFees=-0.05 * 25, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-26', -25,  97.52, TxnFees=-0.05 * 25, verbose=FALSE)
  addTxn(p, "IBM", '2007-01-31', -25,  98.80, TxnFees=-0.05 * 25, verbose=FALSE)

  portfolio <- getPortfolio(p)
  transactions <- portfolio$symbols[["IBM"]]$txn
  checkEquals(-13, sum(transactions$Txn.Fees))
  checkEquals(0, sum(transactions$Txn.Qty))

  # TODO: fix bug in calcPortfSummary
  # summary <- calcPortfSummary(portfolio)
}

test.addTxn_TxnDate_out_of_order_errors <- function() {
  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"))
    try(rm_stocks("A"))
    try(rm(list=paste0("portfolio.",p), pos=.blotter))
  })

  currency("USD")
  stock("A", currency = "USD")
  initPortf("TxnDateOrder", "A")

  # Initialize a portfolio object 'p'
  # Creating portfolio:
  p <- initPortf("runitAddTxn", symbols="A")

  # Trades must be made in date order.
  addTxn(p, "A", "2007-01-04", -50,  97.1)
  checkException(addTxn(p, "A", "2007-01-03",  50,  96.5))
}

test.addTxns <- function() {
  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"), silent=TRUE)
    try(rm_stocks("AMZN"), silent=TRUE)
    try(rm(list=c("account.amzn_acct","portfolio.amzn_txn","portfolio.amzn_txns"), pos=.blotter), silent=TRUE)
  })

  # load the example data
  data("amzn", package="blotter")
  # add transaction fees
  amzn.trades <- merge(amzn.trades, TxnFees=-0.1)
  currency("USD")
  stock("AMZN", currency="USD", multiplier=1)

  # Initialize the account/portfolios
  initAcct("amzn_acct", portfolios=c("amzn_txn","amzn_txns"), initEq=10000)
  initPortf("amzn_txn", symbols="AMZN")
  initPortf("amzn_txns", symbols="AMZN")

  # Add the transactions to the portfolios
  # addTxns:
  addTxns("amzn_txns", "AMZN", TxnData=amzn.trades)
  # addTxn:
  for(i in 1:nrow(amzn.trades)) {
    TxnTime <- index(amzn.trades)[i]
    amzn.trade <- data.frame(amzn.trades[i,])
    with(amzn.trade, addTxn("amzn_txn","AMZN", TxnTime, TxnQty, TxnPrice, TxnFees=TxnFees, verbose=FALSE))
  }

  t1 <- getPortfolio("amzn_txn")$symbols$AMZN$txn
  t2 <- getPortfolio("amzn_txns")$symbols$AMZN$txn
  checkIdentical(t1, t2)
}

test.addTxns_TxnDate_out_of_order_errors <- function() {
  on.exit({
    # remove objects created by unit tests
    try(rm_currencies("USD"), silent=TRUE)
    try(rm_stocks("A"), silent=TRUE)
    try(rm(list=c("account.amzn_acct","portfolio.amzn_txn","portfolio.amzn_txns"), pos=.blotter), silent=TRUE)
  })

  currency("USD")
  stock("AMZN", currency="USD", multiplier=1)

  # Initialize the account/portfolios
  initAcct("amzn_acct", portfolios="amzn_txns", initEq=10000)
  initPortf("amzn_txns", symbols="AMZN")

  # Add the transactions to the portfolios
  addTxns("amzn_txns", "AMZN", TxnData=amzn.trades)
  # Trades must be made in date order.
  checkException(addTxns("amzn_txns", "AMZN", TxnData=amzn.trades[1:2,]))
}

