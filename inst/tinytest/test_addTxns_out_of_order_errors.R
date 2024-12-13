# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea

library(tinytest)

test_addTxns_out_of_order_errors <- function() {

    currency("USD")
  
    data("amzn", package = "blotter")
    amzn <- xts:::.update_index_attributes(amzn)
    amzn.trades <- xts:::.update_index_attributes(amzn.trades)
    
    stock("AMZN", currency = "USD", multiplier = 1)
    
    initAcct("amzn_acct", portfolios = "amzn_txns", initEq = 10000)
    initPortf("amzn_txns", symbols = "AMZN")
    
    addTxns("amzn_txns", "AMZN", TxnData = amzn.trades, verbose = FALSE)
    
    # Trades must be made in date order
    expect_error(addTxns("amzn_txns", "AMZN", TxnData = amzn.trades[1:2,]))
}

test_addTxns_out_of_order_errors()