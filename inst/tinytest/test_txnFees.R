# Author: Peter Carl -> RUnit port by Ben McCann -> 
# -> tinytest port by Justin Shea

library(tinytest)

test_txnFees <- function() {
  try({
    # Set up currency and stock symbols
    currency("USD")
    symbols <- c("IBM")
    for (symbol in symbols) {
      stock(symbol, currency = "USD", multiplier = 1)
    }
    data(IBM, package = "blotter")
    
    # Simple portfolio with one transaction
    p1 <- initPortf(name = "p1runitUpdatePortf", symbols = symbols)
    p1 <- addTxn(
      Portfolio = "p1runitUpdatePortf",
      Symbol = "IBM",
      TxnDate = '2007-01-04',
      TxnQty = 100,
      TxnPrice = 96.5,
      TxnFees = -0.05 * 100,
      verbose = FALSE
    )
    p1 <- updatePortf(Portfolio = "p1runitUpdatePortf", Dates = '2007-01-03::2007-01-10')
    a1 <- initAcct(name = "a1runitUpdatePortf", portfolios = "p1runitUpdatePortf")
    a1 <- updateAcct(a1, '2007-01')
    a1 <- updateEndEq(a1, '2007-01')
    
    # Custom transaction cost function
    fiveCents <- function(qty, prc, ...) {
      return(-0.05 * qty)
    }
    
    p2 <- initPortf(name = "p2runitUpdatePortf", symbols = symbols)
    p2 <- addTxn(
      Portfolio = "p2runitUpdatePortf",
      Symbol = "IBM",
      TxnDate = '2007-01-04',
      TxnQty = 100,
      TxnPrice = 96.5,
      TxnFees = fiveCents,
      verbose = FALSE
    )
    p2 <- updatePortf(Portfolio = "p2runitUpdatePortf", Dates = '2007-01-03::2007-01-10')
    a2 <- initAcct(name = "a2runitUpdatePortf", portfolios = "p2runitUpdatePortf")
    a2 <- updateAcct(a2, '2007-01')
    a2 <- updateEndEq(a2, '2007-01')
    
    # Compare account end equity
    expect_equal(getAccount(a1)$summary$End.Eq, getAccount(a2)$summary$End.Eq)
    
  }, silent = TRUE)
}

test_txnFees()
