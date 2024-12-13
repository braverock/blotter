library(tinytest)

    currency("USD")
    stock("A", currency = "USD")
    p <- blotter::initPortf("TxnDateOrder", symbols = "A")
    
    # Trades must be made in date order
    blotter::addTxn(p, "A", "2007-01-04", -50, 97.1)
    expect_silent(blotter::addTxn(p, "A", "2007-01-05", 50, 96.5))

    