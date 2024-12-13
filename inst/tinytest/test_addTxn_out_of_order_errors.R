# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea

library(tinytest)

.blotter <- new.env()

test_addTxn <- function() {
    currency("USD")
    stock("A", currency = "USD")
    p <- initPortf("TxnDateOutOfOrder", symbols = "A")
    
    addTxn(p, "A", "2007-01-04", -50, 97.1, verbose = FALSE)
    expect_error(addTxn(p, "A", "2007-01-03", 50, 96.5))

}

test_addTxn()