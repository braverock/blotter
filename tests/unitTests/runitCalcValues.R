# Author: Peter Carl, RUnit port by Ben McCann

test.calcTxnValue <- function() {
  checkEquals(99, blotter:::.calcTxnValue(TxnQty=10, TxnPrice=10, TxnFees=1))
}

test.calcTxnAvgCost <- function() {
  checkEquals(9.9, blotter:::.calcTxnAvgCost(TxnValue=99, TxnQty=10))
}

test.calcPosAvgCost <- function() {
  checkEquals(101, blotter:::.calcPosAvgCost(PrevPosQty=10, PrevPosAvgCost=100, TxnValue=1020, PosQty=20))
}

test.calcPosAvgCost_C <- function() {
  checkEquals(101, blotter:::.calcPosAvgCost_C(PrevPosQty=10, PrevPosAvgCost=100, TxnValue=1020, PosQty=20))
}

