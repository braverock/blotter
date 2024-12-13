library(tinytest)

test_calcTxnAvgCost <- function() {
  expect_equal(
    blotter:::.calcTxnAvgCost(TxnValue = 99, TxnQty = 10),
    9.9
  )
}

test_calcTxnAvgCost()