# Author: Peter Carl -> RUnit port by Ben McCann -> 
# -> tinytest port by Justin Shea

library(tinytest)

.blotter <- new.env()

test_addTxns <- function() {
  try({
    data("amzn", package = "blotter")
    
    amzn <- xts:::.update_index_attributes(amzn)
    amzn.trades <- xts:::.update_index_attributes(amzn.trades)
    
    amzn.trades <- merge(amzn.trades, TxnFees = -0.1)
    
    currency("USD")
    stock("AMZN", currency = "USD", multiplier = 1)
    
    initAcct("amzn_acct2", portfolios = c("amzn_txn2", "amzn_txns2"), initEq = 10000)
    initPortf("amzn_txn2", symbols = "AMZN")
    initPortf("amzn_txns2", symbols = "AMZN")
    
    addTxns("amzn_txns2", "AMZN", TxnData = amzn.trades, verbose=FALSE)
    
    for (i in 1:nrow(amzn.trades)) {
      TxnTime <- index(amzn.trades)[i]
      amzn.trade <- data.frame(amzn.trades[i,])
      with(amzn.trade, addTxn("amzn_txn2", "AMZN", TxnTime, TxnQty, TxnPrice, TxnFees = TxnFees, verbose = FALSE ))
    }
    
    t1 <- getPortfolio("amzn_txn2")$symbols$AMZN$txn
    t2 <- getPortfolio("amzn_txns2")$symbols$AMZN$txn
    
    expect_identical(t1, t2)
    
    }, silent = TRUE)
}

test_addTxns()
