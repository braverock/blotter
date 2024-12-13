# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea
library(tinytest)
Sys.setenv(TZ='GMT')

test_addTxn <- function() {

    currency("USD")
    symbol <- c("IBM")
  
    stock(symbol, currency = "USD", multiplier = 1)

    data(IBM, package = "blotter")
    
    # Initialize a portfolio object
    p <- initPortf("runitAddTxn", symbols = symbol)
    
    # Add transactions
addTxn(p, "IBM", '2007-01-03',  50,  96.5,  TxnFees = -0.05 * 50, verbose=FALSE)
addTxn(p, "IBM", '2007-01-04', -50,  97.1,  TxnFees = -0.05 * 50, verbose=FALSE)
addTxn(p, "IBM", '2007-01-08', -10,  99.2,  TxnFees = -0.05 * 10, verbose=FALSE)
addTxn(p, "IBM", '2007-01-09', -10, 100.1,  TxnFees = -0.05 * 10, verbose=FALSE)
addTxn(p, "IBM", '2007-01-17', -10, 100.25, TxnFees = -0.05 * 10, verbose=FALSE)
addTxn(p, "IBM", '2007-01-19',  30,  95,    TxnFees = -0.05 * 30, verbose=FALSE)
addTxn(p, "IBM", '2007-01-22',  25,  96.3,  TxnFees = -0.05 * 25, verbose=FALSE)
addTxn(p, "IBM", '2007-01-23',  25,  96.42, TxnFees = -0.05 * 25, verbose=FALSE)
addTxn(p, "IBM", '2007-01-26', -25,  97.52, TxnFees = -0.05 * 25, verbose=FALSE)
addTxn(p, "IBM", '2007-01-31', -25,  98.80, TxnFees = -0.05 * 25, verbose=FALSE)
    
    portfolio <- getPortfolio(p)
    transactions <- portfolio$symbols[["IBM"]]$txn
    
    expect_equal(sum(transactions$Txn.Fees), -13)
    expect_equal(sum(transactions$Txn.Qty), 0)
    
}

test_addTxn()


