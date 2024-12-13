# Author: Peter Carl -> RUnit port by Ben McCann -> 
# -> tinytest port by Justin Shea

library(tinytest)

    currency("USD")
    stock("AMZN", currency = "USD", multiplier = 1)
    
    initAcct("amzn_acct", portfolios = "amzn_txns", initEq = 10000)
    initPortf("amzn_txns", symbols = "AMZN")
    
    data("amzn", package = "blotter")
    addTxns("amzn_txns", "AMZN", TxnData = amzn.trades)
    
    # Trades must be made in date order
    expect_error(addTxns("amzn_txns", "AMZN", TxnData = amzn.trades[1:2,]))
