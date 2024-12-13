library(tinytest)

    .blotter <- new.env()
    
    currency("USD")
    stock("A", currency = "USD")
    p <- initPortf("TxnDateOutOfOrder", symbols = "A")
    
    addTxn(p, "A", "2007-01-04", -50, 97.1)
    expect_error(addTxn(p, "A", "2007-01-03", 50, 96.5))

    