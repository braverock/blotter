library(tinytest)

test_calcTxnValue <- function() {
  expect_equal(
    blotter:::.calcTxnValue(TxnQty = 10, TxnPrice = 10, TxnFees = 1),
    99
  )
}

test_calcTxnValue()