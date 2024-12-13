# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea

library(tinytest)

test_addTxn <- function() {
 
    currency("USD")
    stock("A", currency = "USD")
    portfolio <- blotter::initPortf("TxnDateOrder", symbols = "A")
    
    # Trades must be made in date order
    blotter::addTxn(portfolio, "A", "2007-01-04", -50, 97.1, verbose = FALSE)
    expect_silent(blotter::addTxn(portfolio, "A", "2007-01-05", 50, 96.5))
}

test_addTxn()