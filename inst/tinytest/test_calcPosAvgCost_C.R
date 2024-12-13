library(tinytest)

test_calcPosAvgCost_C <- function() {
  expect_equal(
    blotter:::.calcPosAvgCost_C(
      PrevPosQty = 10,
      PrevPosAvgCost = 100,
      TxnValue = 1020,
      PosQty = 20
    ),
    101
  )
}

test_calcPosAvgCost_C()