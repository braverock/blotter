# Author: Peter Carl -> RUnit port by Ben McCann -> tinytest port by Justin Shea
library(tinytest)
Sys.setenv(TZ='GMT')

test_addTxns <- function() {

    currency("USD")
    
    data("amzn", package = "blotter")

    amzn.trades <- merge(amzn.trades, TxnFees = -0.1)
    
    stock("amzn", currency = "USD", multiplier = 1)
    
    initAcct("amzn_acct2", portfolios = c("amzn_txn2", "amzn_txns2"), initEq = 10000)
    initPortf("amzn_txn2", symbols = "amzn")
    initPortf("amzn_txns2", symbols = "amzn")
    
    addTxns("amzn_txns2", "amzn", TxnData = amzn.trades, verbose=FALSE)
    
    for (i in 1:nrow(amzn.trades)) {
      TxnTime <- index(amzn.trades)[i]
      amzn.trade <- data.frame(amzn.trades[i,])
      with(amzn.trade, addTxn("amzn_txn2", "amzn", TxnTime, TxnQty, TxnPrice, TxnFees = TxnFees, verbose = FALSE ))
    }
    
    t1 <- getPortfolio("amzn_txn2")$symbols$amzn$txn
    t2 <- getPortfolio("amzn_txns2")$symbols$amzn$txn
    
    expect_identical(t1, t2)

}

test_addTxns()


