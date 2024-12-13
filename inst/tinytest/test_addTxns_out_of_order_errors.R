# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea
library(tinytest)
Sys.setenv(TZ='GMT')

test_addTxns_out_of_order_errors <- function() {

    currency("USD")
  
    data("amzn", package = "blotter")

    stock("amzn", currency = "USD", multiplier = 1)
    
    initAcct("amzn_acct", portfolios = "amzn_txns", initEq = 10000)
    initPortf("amzn_txns", symbols = "amzn")
    
    addTxns("amzn_txns", "amzn", TxnData = amzn.trades, verbose = FALSE)
    
    # Trades must be made in date order
    expect_error(addTxns("amzn_txns", "amzn", TxnData = amzn.trades[1:2,]))

    }

test_addTxns_out_of_order_errors()


